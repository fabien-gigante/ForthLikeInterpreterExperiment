''' FOLIE : FOrth-Like Interpreter Experiment '''

from typing import Dict, Iterable, Optional, Tuple, Union, List, Type
import os
import colorama
from colorama import Fore as fg


class Error(Exception):
    ''' Applicative Error. Rendered in red. '''
    def __init__(self, msg) -> None:
        super().__init__(f'{fg.LIGHTRED_EX}ERROR:{fg.RESET} {msg}')

class Atom:
    ''' Abstract. Smallest element of language. '''
    def unbox(self) -> Iterable['Atom']:
        raise Error(f'atom {self} cannot be unboxed')
    def execute(self, runtime: 'Runtime') -> None:
        raise Error(f'atom {self} cannot be executed')

class Literal(Atom):
    ''' Abstract. Atomic int, float or string literal value. '''
    def unbox(self) -> Iterable[Atom]: yield self
    def execute(self, runtime: 'Runtime') -> None:
        runtime.push(self)

class NumberLiteral(Literal):
    ''' Atomic int or float literal value. '''
    def __init__(self, value: Union[int,float]) -> None:
        self.value = value
    def __str__(self) -> str:
        return f'{fg.CYAN}{self.value}{fg.RESET}'

class StringLiteral(Literal):
    ''' Atomic string literal value. '''
    def __init__(self, value: str) -> None:
        self.value = value
    def __str__(self) -> str:
        return f"{fg.CYAN}'{self.value}'{fg.RESET}"

class Comment(Atom):
    ''' A piece of comment string. '''
    def __init__(self, value: str) -> None:
        self.value = value
    def __str__(self) -> str:
        return f'{fg.GREEN}({self.value}){fg.RESET}'
    def execute(self, runtime: 'Runtime') -> None: pass

class Keyword(Atom):
    ''' Intermediate output of the Tokenizer. Not a proper language element. '''
    def __init__(self, value: str) -> None:
        self.value = value
    def __str__(self) -> str:
        return f'{fg.MAGENTA}<{self.value}>{fg.RESET}'

class Word(Atom):
    ''' Fundamental runtime executable token. '''
    def __init__(self, value: str) -> None:
        self.value = value
    def __str__(self) -> str:
        return f'{fg.YELLOW}{self.value}{fg.RESET}'
    def execute(self, runtime: 'Runtime') -> None:
        runtime.execute(self.value)

class Sequence(Atom):
    ''' List of atoms. '''
    def __init__(self, content: Iterable[Atom]):
        self.content = [*content]
    def __str__(self) -> str:
        return '{ ' + ' '.join( f'{atom}' for atom in self.content ) + ' }'
    def execute(self, runtime: 'Runtime') -> None:
        runtime.push(self)
    def unbox(self) -> Iterable[Atom]: yield from self.content

class Intrinsic(Atom):
    ''' Intrinsic implementation of a word. '''
    classes : List[Type['Intrinsic']] = []
    def __init_subclass__(cls) -> None: Intrinsic.classes.append(cls)
    def __init__(self, value: str, comment: Optional[str] = None):
        self.value = value
        self.comment = comment
    def register(self, runtime: 'Runtime'):
        if self.comment is None :
            runtime.register(self.value, self)
        else:
            runtime.register(self.value, Sequence([Comment(self.comment), self]))
    def __str__(self) -> str:
        return f'{fg.LIGHTBLACK_EX}intrinsic<{type(self).__name__}>{fg.RESET}'

# Token are outputs of the Tokenizer
Token = Union[ Literal, Comment, Keyword ]

class Tokenizer:
    '''
    Syntaxical tokenizer. Turns a string into a series of tokens.
    Can only produce Literal, Comment, Keyword.
    '''

    def __init__(self, input_str: str) -> None:
        self.input = input_str

    @staticmethod
    def is_separator(c: str) -> bool:
        return c.isspace() or c in (':', ';', '{', '}', "'", '"', '(', ')')

    @staticmethod
    def is_number(s: str) -> bool:
        try: int(s)
        except ValueError: pass
        else: return True
        try: float(s)
        except ValueError: pass
        else: return True
        return False

    def parse_string(self, prefix: str, suffix: str) -> Optional[str]:
        if self.input[0] != prefix: return None
        idx = self.input.find(suffix, 1)
        if idx < 0: raise Error(f'missing closing {suffix}')
        token = self.input[1:idx] ; self.input = self.input[idx+1:]
        return token

    def parse_until_separator(self) -> str:
        idx = 0
        while idx < len(self.input) and not Tokenizer.is_separator(self.input[idx]): idx += 1
        if idx == 0: token = self.input[0] ; self.input = self.input[1:]
        else: token = self.input[:idx] ; self.input = self.input[idx:]
        return token

    def tokenize(self) -> Iterable[Token]:
        while True:
            self.input = self.input.lstrip()
            if len(self.input) == 0: break
            token = self.parse_string('"', '"')
            if token is not None: yield StringLiteral(token); continue
            token = self.parse_string("'", "'")
            if token is not None: yield StringLiteral(token); continue
            token = self.parse_string('(', ')')
            if token is not None: yield Comment(token); continue
            token = self.parse_until_separator()
            try: yield NumberLiteral(int(token))
            except ValueError:
                try: yield NumberLiteral(float(token))
                except ValueError:
                    yield Keyword(token.lower())

class Pattern:
    '''
    Abstract. A pattern is a known sequence of token / keywords that can be handled by the parser.
    It has the form : prefix ... suffix. Parsing implementation is provided in Pattern sub classes.
    '''
    classes : List[Type['Pattern']] = []
    def __init_subclass__(cls) -> None: Pattern.classes.append(cls)
    def __init__(self, prefix: str, suffix: str) -> None:
        self.prefix = prefix ; self.suffix = suffix
    def parse(self, parser: 'Parser') -> Iterable[Atom]:
        pass
    def reserved(self) -> Iterable[str]:
        yield self.prefix
        yield self.suffix
    def __str__(self) -> str:
        return f'{self.prefix}{fg.LIGHTBLACK_EX} .. {fg.RESET}{self.suffix}'

class Parser:
    '''
    Gramatical parser.
    Parses a string or a series of tokens. Resolves any Keyword into Words or Sequences.
    Can therefore only produce Litteral, Comment, Word or Sequence.
    '''

    def __init__(self) -> None:
        self.input : Optional[Iterable[Token]] = None
        self.patterns : Dict[str, Pattern] = {}
        self.closure: Optional[str] = None

    def register(self, pattern: Pattern) -> None:
        self.patterns[pattern.prefix] = pattern

    def parse(self, input_str: str) -> Iterable[Atom]:
        self.input = Tokenizer(input_str).tokenize()
        yield from self.parse_many()

    def next(self) -> Optional[Token]:
        if self.input is None: return None
        return next(self.input, None)

    def parse_many(self, closure: Tuple[Optional[str],...] = (None, ), ignore: Tuple[str,...] = ()) -> Iterable[Atom]:
        self.closure = None
        while True:
            token = self.next()
            if token is None and None in closure: break
            if token is None: raise Error(f'missing closing {closure}')
            if isinstance(token, Keyword) and token.value in closure:
                self.closure = token.value ; break
            if isinstance(token, Keyword) and token.value in ignore:
                yield token
            else:
                yield from self.parse_token(token)

    def reserved_patterns(self) -> Iterable[str]:
        for p in self.patterns.values(): yield from p.reserved()

    def check_valid_word(self, word: str) -> None:
        if any(Tokenizer.is_separator(ch) for ch in word):
            raise Error(f'word name {word} contains an invalid character')
        if word in self.reserved_patterns():
            raise Error(f'word name {word} is reserved')
        if Tokenizer.is_number(word):
            raise Error(f'word name {word} is a number')

    def parse_token(self, token: Token) -> Iterable[Atom]:
        if not isinstance(token, Keyword):
            yield token ; return
        if token.value in self.patterns:
            yield from self.patterns[token.value].parse(self)
        else:
            self.check_valid_word(token.value)
            yield Word(token.value)

    def execute(self, runtime: 'Runtime', input_str: str) -> None:
        atoms = [*self.parse(input_str)]
        for atom in atoms: atom.execute(runtime)

class SequencePattern(Pattern):
    ''' Pattern { ... } used to define a sequence. '''
    def __init__(self) : super().__init__('{', '}')
    def parse(self, parser: Parser) -> Iterable[Atom]:
        yield Sequence(parser.parse_many((self.suffix,)))

class DefinePattern(Pattern):
    ''' Pattern : ... ; used to define a new word. '''
    def __init__(self) : super().__init__(':', ';')
    def parse(self, parser: Parser) -> Iterable[Atom]:
        word = parser.next()
        if word is None: raise Error('missing word in definition')
        if not isinstance(word, Keyword): raise Error(f'invalid word type {word}')
        parser.check_valid_word(word.value)
        yield Sequence(parser.parse_many((self.suffix,)))
        yield Sequence([Word(word.value)])
        yield Word('def')

class IfPattern(Pattern):
    ''' Pattern if ... else ... then, used to define conditional logic. '''
    def __init__(self) :
        super().__init__('if', 'then')
    def parse(self, parser: Parser) -> Iterable[Atom]:
        then_cont = [*parser.parse_many((self.suffix,), ('else',))]
        else_cont = None
        for i, atom in enumerate(then_cont):
            if isinstance(atom, Keyword) and atom.value == 'else':
                (then_cont, else_cont) = (then_cont[:i], then_cont[i+1:])
                break
        if else_cont is None:
            yield from (Sequence(then_cont), Word('swap'), Word('?if'))
        else:
            if any(isinstance(atom, Keyword) for atom in else_cont):
                raise Error('A single else keyword is allowed in an if statement')
            yield from (Sequence(then_cont), Sequence(else_cont), Word('rot'), Word('?ifelse'))
    def reserved(self) -> Iterable[str]:
        yield from super().reserved()
        yield 'else'
    def __str__(self) -> str:
        return f'{self.prefix}{fg.LIGHTBLACK_EX} .. {fg.RESET}[else {fg.LIGHTBLACK_EX}..{fg.RESET}] {self.suffix}'

class BeginPattern(Pattern):
    ''' Pattern begin ... again, for forever loops. '''
    def __init__(self) :
        super().__init__('begin', 'again')
    def parse(self, parser: Parser) -> Iterable[Atom]:
        content = [*parser.parse_many((self.suffix, 'until', 'repeat'), ('while',))]
        if parser.closure == 'until': 
            content += [ Word('?leave') ]
        else:
            for i, atom in enumerate(content):
                if isinstance(atom, Keyword) and atom.value == 'while':
                    content = [ *content[:i], Word('not'), Word('?leave'), *content[i+1:] ]
                    break
        if any(isinstance(atom, Keyword) for atom in content):
            raise Error('A single until or while keyword is allowed in a begin statement')
        yield Sequence(content)
        yield Word('forever')
    def reserved(self) -> Iterable[str]:
        yield from super().reserved()
        yield 'until' ; yield 'while' ; yield 'repeat'
    def __str__(self) -> str:
        return super().__str__() + f' | until | while {fg.LIGHTBLACK_EX}..{fg.RESET} repeat'

class Runtime:
    '''
    Runtime environement for execution.
    Holds and manages the list of available words and the stack.
    '''

    def __init__(self) -> None:
        self.words: Dict[str, Atom] = {}
        self.stack: List[Atom] = []

    def check_type(self, atom: Atom, atom_type: Union[None, Type[Atom], Tuple[Type[Atom],...]]) -> None:
        if atom_type is None or isinstance(atom, atom_type): return
        if isinstance(atom_type, type):
            raise Error(f'argument {atom} is not a {atom_type.__name__.lower()}')
        raise Error(f'argument {atom} is not one of {" , ".join(t.__name__.lower() for t in atom_type)}')

    def peek(self, i: int = 0) -> Atom : return self.stack[-(i+1)]
    def pop(self, atom_type: Union[None, Type[Atom], Tuple[Type[Atom],...]] = None) -> Atom:
        if len(self.stack) == 0: raise Error('empty stack')
        self.check_type(self.stack[-1], atom_type)
        return self.stack.pop()

    def pop_args(self, types: List[Union[None, Type[Atom], Tuple[Type[Atom],...]]], matching: bool = False) -> Iterable[Atom]:
        n = len(types)
        if len(self.stack) < n: raise Error(f'need {n} arguments')
        for i, t in enumerate(types): self.check_type(self.peek(i), t)
        if matching and len({ type(arg) for arg in self.stack[-n:] }) != 1:
            raise Error('arguments types no not match')
        for _ in range(n): yield self.pop()

    def push(self, atom: Atom) -> None:
        self.stack.append(atom)

    def is_intrinsic(self, name: str) -> bool:
        if not name in self.words: return False
        word = self.words[name]
        return any(isinstance(atom, Intrinsic) for atom in word.unbox())

    def register(self, word: str, definition: Atom) -> None:
        self.words[word] = definition

    def execute(self, word: str) -> None:
        if not word in self.words: raise Error(f'unknown word {Word(word)}')
        for atom in self.words[word].unbox(): atom.execute(self)

    def describe(self, word: str) -> str:
        if not word in self.words: raise Error(f'unknown word {Word(word)}')
        return ' '.join(f'{word}' for word in self.words[word].unbox())

#
#   All intrinsic implementations
#

class Print(Intrinsic):
    def __init__(self): super().__init__('.','a --  , print a')
    def execute(self, runtime: Runtime) -> None:
        print(f'  = {runtime.pop()}')

class Help(Intrinsic):
    def __init__(self): super().__init__('.w', 'print patterns and words')
    def execute(self, runtime: Runtime) -> None:
        for pattern in Pattern.classes:
            print(f'  {pattern()}   {fg.LIGHTBLACK_EX}pattern<{pattern.__name__}>{fg.RESET}')
        for key in sorted(runtime.words.keys()):
            print(f'  : {fg.YELLOW}{key}{fg.RESET} {runtime.describe(key)} ;')

class PrintStack(Intrinsic):
    def __init__(self): super().__init__('.s', 'print stack')
    def execute(self, runtime: Runtime) -> None:
        print()
        i = len(runtime.stack)
        for atom in runtime.stack:
            print(f'{fg.LIGHTBLACK_EX}  {i} :{fg.RESET}\r\t\t{atom}')
            i -= 1

class Define(Intrinsic):
    def __init__(self): super().__init__('def', 'a { b } --  , define b as a')
    def execute(self, runtime: Runtime) -> None:
        quote, definition = runtime.pop_args([Sequence, None])
        word = quote.content[0] if len(quote.content) == 1 else None
        if word is None: 
            raise Error(f'invalid argument {quote} in definition')
        if runtime.is_intrinsic(word.value):
            raise Error(f'cannot redefine intrinsic {word}')
        runtime.register(word.value, definition)

class Add(Intrinsic):
    def __init__(self): super().__init__('+', 'a b -- a+b')
    def execute(self, runtime: Runtime) -> None:
        arg2, arg1 = runtime.pop_args([(Literal, Sequence), (Literal, Sequence)], True)
        if isinstance(arg1, Sequence):
            runtime.push(Sequence([*arg1.content, *arg2.content]))
        elif isinstance(arg1, NumberLiteral) and isinstance(arg2, NumberLiteral):
            runtime.push(NumberLiteral(arg1.value + arg2.value))
        elif isinstance(arg1, StringLiteral) and isinstance(arg2, StringLiteral):
            runtime.push(StringLiteral(arg1.value + arg2.value))

class Substract(Intrinsic):
    def __init__(self): super().__init__('-', 'a b -- a-b')
    def execute(self, runtime: Runtime) -> None:
        arg2, arg1 = runtime.pop_args([NumberLiteral, NumberLiteral])
        runtime.push(NumberLiteral(arg1.value - arg2.value))

class Multiply(Intrinsic):
    def __init__(self): super().__init__('*', 'a b -- a*b')
    def execute(self, runtime: Runtime) -> None:
        arg2, arg1 = runtime.pop_args([(NumberLiteral, Sequence), (NumberLiteral, Sequence)])
        if isinstance(arg1, NumberLiteral) and isinstance(arg2, NumberLiteral):
            runtime.push(NumberLiteral(arg1.value * arg2.value))
        elif isinstance(arg1, Sequence) and isinstance(arg2, NumberLiteral) and isinstance(arg2.value, int):
            runtime.push(Sequence(arg1.content * arg2.value))
        elif isinstance(arg2, Sequence) and isinstance(arg1, NumberLiteral) and isinstance(arg1.value, int):
            runtime.push(Sequence(arg1.value * arg2.content))
        else: raise Error(f'Invalid argument types for {self.value}')

class Divide(Intrinsic):
    def __init__(self): super().__init__('/', 'a b -- a/b')
    def execute(self, runtime: Runtime) -> None:
        arg2, arg1 = runtime.pop_args([NumberLiteral, NumberLiteral])
        runtime.push(NumberLiteral(arg1.value / arg2.value))

class Equals(Intrinsic):
    def __init__(self): super().__init__('=', 'a b -- a=b')
    def execute(self, runtime: Runtime) -> None:
        arg2, arg1 = runtime.pop_args([Literal, Literal])
        runtime.push(NumberLiteral(1 if arg1.value == arg2.value else 0))

class LowerThan(Intrinsic):
    def __init__(self): super().__init__('<', 'a b -- a<b')
    def execute(self, runtime: Runtime) -> None:
        arg2, arg1 = runtime.pop_args([Literal, Literal], True)
        runtime.push(NumberLiteral(1 if arg1.value < arg2.value else 0))

class GreaterThan(Intrinsic):
    def __init__(self): super().__init__('>', 'a b -- a>b')
    def execute(self, runtime: Runtime) -> None:
        arg2, arg1 = runtime.pop_args([Literal, Literal], True)
        runtime.push(NumberLiteral(1 if arg1.value > arg2.value else 0))

class Clear(Intrinsic):
    def __init__(self): super().__init__('clear', 'a1 .. an --')
    def execute(self, runtime: Runtime) -> None:
        runtime.stack = []

class Depth(Intrinsic):
    def __init__(self): super().__init__('depth', 'a1 .. an -- a1 .. an n')
    def execute(self, runtime: Runtime) -> None:
        runtime.push(NumberLiteral(len(runtime.stack)))

class Evaluate(Intrinsic):
    def __init__(self): super().__init__('eval', 'a -- eval of a' )
    def execute(self, runtime: Runtime) -> None:
        arg = runtime.pop()
        for atom in arg.unbox(): atom.execute(runtime)

class LoopInterrupt(Exception): pass

class Leave(Intrinsic):
    def __init__(self): super().__init__('leave', 'interrupts loop')
    def execute(self, runtime: Runtime) -> None: raise LoopInterrupt()

class Forever(Intrinsic):
    def __init__(self): super().__init__('forever', 'a -- , evaluates a forever' )
    def execute(self, runtime: Runtime) -> None:
        arg = runtime.pop(Sequence)
        try:
            while True:
                runtime.push(arg)
                runtime.execute('eval')
        except LoopInterrupt: pass

class EvaluateIf(Intrinsic):
    def __init__(self): super().__init__('?if', 'a b -- eval of a if b')
    def execute(self, runtime: Runtime) -> None:
        cond = runtime.pop(NumberLiteral)
        if cond.value == 0: runtime.pop()
        else: runtime.execute('eval')

class Prepend(Intrinsic):
    def __init__(self): super().__init__('<+', 'a1 {a2 .. an} -- {a1 a2 .. an}')
    def execute(self, runtime: Runtime) -> None:
        seq, atom = runtime.pop_args([Sequence, None])
        runtime.push( Sequence([atom, *seq.content]) )

class Postpend(Intrinsic):
    def __init__(self): super().__init__('+>', 'a1 {a2 .. an} -- {a2 .. an a1}')
    def execute(self, runtime: Runtime) -> None:
        seq, atom = runtime.pop_args([Sequence, None])
        runtime.push( Sequence([*seq.content, atom]) )

class Reverse(Intrinsic):
    def __init__(self): super().__init__('reverse', '{a1 .. an} -- {an .. a1}')
    def execute(self, runtime: Runtime) -> None:
        seq = runtime.pop(Sequence)
        runtime.push( Sequence(reversed(seq.content)) )

class Accept(Intrinsic):
    def __init__(self): super().__init__('accept', ' -- a ; user entered string')
    def execute(self, runtime: Runtime) -> None:
        runtime.push( StringLiteral(input('  ? ')) )

class Page(Intrinsic):
    def __init__(self): super().__init__('page', 'clear screen')
    def execute(self, runtime: Runtime) -> None: print('\033[2J', end='')

class Interpreter:
    ''' The interpreter program '''

    def __init__(self) -> None:
        os.system('')
        colorama.init(convert = True, strip = False)
        self.prompt = fg.LIGHTWHITE_EX + '> ' + fg.RESET
        self.showstack = True
        self.runtime = Runtime()
        for intrinsic in Intrinsic.classes: intrinsic().register(self.runtime)
        self.parser = Parser()
        for pattern in Pattern.classes: self.parser.register(pattern())
        for instruction in Interpreter.BOOTSTRAP: self.execute(instruction)
        print(f"Welcome to {fg.LIGHTWHITE_EX}FOLIE{fg.RESET} {Comment('FOrth-Like Interpreter Experiment')}.")
        print(f'Type {Word("help")} for available patterns and verbs.\n\nExamples:')
        print(f'  {fg.LIGHTBLACK_EX}:! (a -- a!)  dup 0 > if dup 1 - ! * else drop 1 then;{fg.RESET}')
        print(f'  {fg.LIGHTBLACK_EX}:! (a -- a!) 1 begin over 0 > while over 1 - -rot * repeat nip;{fg.RESET}')
        print(f'  {fg.LIGHTBLACK_EX}6 ! .{fg.RESET}')

    def execute(self, expression: str) -> None:
        self.parser.execute(self.runtime, expression)

    BOOTSTRAP = [
        ':print (alias) .; :stack (alias) .s; :help (alias) .w; :bye (alias) leave;',
        ':prepend (alias) <+; :append (alias) +>;',
        ':drop (a --) {} +> 0 * eval;',
        ':dup (a -- a a) {} +> 2 * eval;',
        ':swap (a b -- b a) {} +> +> eval;',
        ':rot (a b c -- b c a) {} +> <+ +> eval;',
        ':-rot (a b c -- c a b) rot rot;',
        ':nip (a b -- b) swap drop;',
        ':tuck (a b -- b a b) dup -rot;',
        ':over (a b -- a b a) swap tuck;',
        ':neg (a -- -a) 0 swap -;',
        ':not (a -- boolean not of a) 0 1 rot ?ifelse;',
        ':<= (a b -- a<=b) > not; :>= (a b -- a>=b) < not; :<> (a b -- a<>b) = not;',
        ':!= (alias) <>; : == (alias) =;',
        ':?ifelse (a b c -- a !, if c | b !, otherwise) {swap} swap ?if swap drop eval;',
        ':?leave ( a -- interrupts if a) if leave then;'
    ]

    def loop(self) -> None:
        while True :
            if self.showstack: self.execute('.s')
            try:
                self.execute(input(self.prompt))
            except Error as error:
                print(error)
            except KeyboardInterrupt:
                print(Error('execution interupted by user'))
            except LoopInterrupt: 
                break

# Main function calling
if __name__ == '__main__':
    Interpreter().loop()
