from typing import Dict, Iterable, Optional, Tuple, Union, List, Type
import os
import colorama
from colorama import Fore as fg


class Error(Exception):
    ''' Applicative Error. Rendered in red. '''
    def __init__(self, msg) -> None:
        super().__init__(f'{fg.RED}ERROR:{fg.RESET} {msg}')

class Atom:
    ''' Abstract. Smallest element of language. '''
    def execute(self, runtime: 'Runtime') -> None:
        raise Error(f'atom {self} cannot be executed')

class Token(Atom):
    ''' Abstract. A Token is an Atom with a value. '''
    def __init__(self, value: Union[int,float,str]) -> None:
        self.value = value

class Literal(Token):
    ''' Abstract. Atomic int, float or string literal value. '''
    def __str__(self) -> str:
        if isinstance(self.value, str):
            return f"{fg.CYAN}'{self.value}'{fg.RESET}"
        else:
            return f'{fg.CYAN}{self.value}{fg.RESET}'

    def execute(self, runtime: 'Runtime') -> None:
        runtime.push(self)

class NumberLiteral(Literal):
    ''' Atomic int or float literal value. '''

class StringLiteral(Literal):
    ''' Atomic string literal value. '''
    pass

class Comment(Token):
    ''' A piece of comment string. '''
    def __str__(self) -> str:
        return f'{fg.GREEN}({self.value}){fg.RESET}'

    def execute(self, runtime: 'Runtime') -> None:
        pass

class Keyword(Token):
    ''' Intermediate output of the Tokenizer. Not a language element. '''
    def __str__(self) -> str:
        return f'{fg.MAGENTA}<{self.value}>{fg.RESET}'

class Quote(Token):
    ''' A reference to a word by its name. '''
    def __str__(self) -> str:
        return f'{fg.YELLOW}`{self.value}`{fg.RESET}'
    def execute(self, runtime: 'Runtime') -> None:
        runtime.push(self)

class Word(Token):
    ''' Fundamental runtime executable token. '''
    def __str__(self) -> str:
        return f'{fg.YELLOW}{self.value}{fg.RESET}'
    def execute(self, runtime: 'Runtime') -> None:
        runtime.execute(self.value)

class Sequence(Atom):
    ''' List of atoms. '''
    def __init__(self, content: Iterable[Atom]):
        super().__init__()
        self.content = [*content]
    def __str__(self) -> str:
        return '{ ' + ' '.join( f'{atom}' for atom in self.content ) + ' }'
    def execute(self, runtime: 'Runtime') -> None:
        runtime.push(self)

class Intrinsic(Token):
    ''' Intrinsic implementation of a word. '''
    all = []
    def __init_subclass__(cls) -> None: Intrinsic.all.append(cls)
    def __init__(self, value: str, comment: Optional[str] = None):
        super().__init__(value)
        self.comment = comment
    def register(self, runtime: 'Runtime'):
        if self.comment is None :
            runtime.register(self.value, [self])
        else:
            runtime.register(self.value, [Comment(self.comment), self])
    def __str__(self) -> str:
        return f'{fg.LIGHTBLACK_EX}intrinsic<{type(self).__name__}>{fg.RESET}'

class Tokenizer:
    ''' 
    Syntaxical tokenizer. 
    Turns a string into a series of tokens. 
    Can only produce Litteral, Comment, Quote or Keyword.
    '''

    def __init__(self, input_str: str) -> None:
        self.input = input_str

    @staticmethod
    def is_separator(c: str) -> bool:
        return c.isspace() or c in (':', ';', '{', '}', '`', "'", '"', '(', ')', '`')

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
            token = self.parse_string('`', '`')
            if token is not None: yield Quote(token); continue
            token = self.parse_until_separator()
            try: yield NumberLiteral(int(token))
            except ValueError:
                try: yield NumberLiteral(float(token))
                except ValueError:
                    yield Keyword(token.lower())

class Pattern:
    '''
    Abstract. A pattern is a known sequence of token / keywords that can be handled by the parser.
    It has the form : prefix ... [middle] ... suffix. Parsing implementation is provided in Pattern sub classes.
    '''
    all = []
    def __init_subclass__(cls) -> None: Pattern.all.append(cls)
    def __init__(self, prefix: str, suffix: str, middle: Optional[str] = None) -> None:
        self.prefix = prefix ; self.suffix = suffix ; self.middle = middle
    def parse(self, parser: 'Parser') -> Iterable[Token]:
        pass
    def __str__(self) -> str:
        if self.middle is None:
            return f'{self.prefix}{fg.LIGHTBLACK_EX} .. {fg.RESET}{self.suffix}'
        else:
            return f'{self.prefix}{fg.LIGHTBLACK_EX} .. {fg.RESET}{self.middle}{fg.LIGHTBLACK_EX} .. {fg.RESET}{self.suffix}'


class Parser:
    ''' 
    Gramatical parser. 
    Parses a string or a series of tokens. Resolves any Keyword into Words or Sequences. 
    Can therefore only produce Litteral, Comment, Quote or Word or Sequence.
    '''

    def __init__(self) -> None:
        self.input : Optional[Iterable[Token]] = None
        self.patterns : Dict[str, Pattern] = {}

    def register(self, pattern: Pattern) -> None:
        self.patterns[pattern.prefix] = pattern

    def parse(self, input_str: str) -> Iterable[Atom]:
        self.input = Tokenizer(input_str).tokenize()
        yield from self.parse_many()

    def next(self) -> Optional[Token]:
        return next(self.input, None)

    def parse_many(self, closure: Optional[str] = None, ignore: Tuple[str] = ()) -> Iterable[Atom]:
        while True:
            token = self.next()
            if token is None and closure is None: break
            if token is None: raise Error(f'missing closing {closure}')
            if isinstance(token, Keyword) and token.value == closure: break
            if isinstance(token, Keyword) and token.value in ignore: yield token
            else: yield from self.parse_token(token)

    def reserved_patterns(self) -> Iterable[str]:
        for p in self.patterns.values():
            yield from (p.prefix, p.suffix, p.middle)

    def check_valid_word(self, word: str) -> None:
        if any(Tokenizer.is_separator(ch) for ch in word):
           raise Error(f'word name {token} contains an invalid character')
        if word in self.reserved_patterns():
            raise Error(f'word name {token} is reserved')
        if Tokenizer.is_number(word):
            raise Error(f'word name {token} is a number')
        return True

    def parse_token(self, token: Token) -> Iterable[Atom]:
        if not isinstance(token, Keyword): 
            yield token ; return
        if token.value in self.patterns:
            yield from self.patterns[token.value].parse(self)
        else: 
            self.check_valid_word(token.value)
            yield Word(token.value)

    def execute(self, runtime: 'Runtime', input_str: str) -> None:
        atoms = self.parse(input_str)
        for atom in atoms: atom.execute(runtime)

class SequencePattern(Pattern):
    ''' Pattern { ... } used to define a sequence. '''
    def __init__(self) : super().__init__('{', '}')
    def parse(self, parser: Parser) -> Iterable[Token]:
        yield Sequence(parser.parse_many(self.suffix))

class DefinePattern(Pattern):
    ''' Pattern : ... ; used to define a new word. '''
    def __init__(self) : super().__init__(':', ';')
    def parse(self, parser: Parser) -> Iterable[Token]:
        word = parser.next()
        if word is None: raise Error('missing word in definition')
        if not isinstance(word, Keyword): raise Error(f'invalid word type {word}')
        parser.check_valid_word(word.value)
        yield Sequence(parser.parse_many(self.suffix))
        yield Quote(word.value)
        yield Word('def')

class IfPattern(Pattern):
    ''' Pattern if ... else ... then used to define conditional logic. '''
    def __init__(self) :
        super().__init__('if', 'then', 'else')
    def parse(self, parser: Parser) -> Iterable[Token]:
        then_content = [*parser.parse_many(self.suffix, (self.middle,))]
        else_content = None
        for i in range(len(then_content)):
            if isinstance(then_content[i], Keyword) and then_content[i].value == self.middle:
                (then_content, else_content) = (then_content[:i], then_content[i+1:])
                break
        if else_content is None:
            yield from (Sequence(then_content), Word('swap'), Word('?!'))
        else:
            yield from (Sequence(then_content), Sequence(else_content), Word('rot'), Word('?ifelse'))

class Runtime:
    '''
    Runtime environement for execution.
    Holds and manages the list of available words and the stack.
    '''

    def __init__(self) -> None:
        self.words: Dict[str, List[Atom]] = {}
        self.stack: List[Atom] = []
        self.halted = False

    def check_type(self, atom: Atom, atom_type: Union[Type[Atom], Tuple[Type[Atom]], None]) -> None:
        if atom_type is None or isinstance(atom, atom_type): return
        if isinstance(atom_type, type):
            raise Error(f'argument {atom} is not a {atom_type.__name__.lower()}')
        else:
            raise Error(f'argument {atom} is not one of {" , ".join(t.__name__.lower() for t in atom_type)}')

    def peek(self, i: int = 0) -> Atom : return self.stack[-(i+1)]
    def pop(self, atom_type: Union[Type[Atom],Tuple[Type[Atom]],None] = None) -> Atom:
        if len(self.stack) == 0: raise Error('empty stack')
        self.check_type(self.stack[-1], atom_type)
        return self.stack.pop()

    def pop_args(self, types: List[Union[Type[Atom], Tuple[Type[Atom]], None]], matching: bool = False) -> Iterable[Atom]:
        n = len(types)
        if len(self.stack) < n: raise Error(f'need {n} arguments')
        for i, t in enumerate(types): self.check_type(self.peek(i), t)
        if matching and len({ type(arg) for arg in self.stack[-n:] }) != 1:
            raise Error(f'arguments types no not match')
        for _ in range(n): yield self.pop()

    def push(self, atom: Atom) -> None:
        self.stack.append(atom)

    def is_intrinsic(self, word: str) -> bool:
        if not word in self.words: return False
        return all(isinstance(atom, (Comment, Intrinsic)) for atom in self.words[word])

    def register(self, word: str, definition: List[Atom]) -> None:
        self.words[word] = [*definition]

    def execute(self, word: str) -> None:
        if not word in self.words: raise Error(f'unknown word {Word(word)}')
        for atom in self.words[word]: atom.execute(self)

    def describe(self, word: str) -> str:
        return ' '.join( f'{atom}' for atom in self.words[word] )

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
        for pattern in Pattern.all:
            print(f'  {pattern()}\r\t\t\t{fg.LIGHTBLACK_EX}pattern<{pattern.__name__}>{fg.RESET}')
        for key in sorted(runtime.words.keys()):
            print(f'  : {fg.YELLOW}{key}{fg.RESET}\r\t\t{runtime.describe(key)} ;')

class PrintStack(Intrinsic):
    def __init__(self): super().__init__('.s', 'print stack')
    def execute(self, runtime: Runtime) -> None:
        i = len(runtime.stack)
        for atom in runtime.stack:
            print(f'{fg.LIGHTBLACK_EX}  {i} :{fg.RESET}\r\t\t{atom}')
            i -= 1

class Define(Intrinsic):
    def __init__(self): super().__init__('def', '{a1 .. an} b --  , define b as a1 .. an')
    def execute(self, runtime: Runtime) -> None:
        quote, definition = runtime.pop_args([Quote, Sequence])
        word = quote.value
        if any(Tokenizer.is_separator(ch) for ch in word):
            raise Error(f'invalid character in word {quote}')
        if runtime.is_intrinsic(word):
            raise Error(f'cannot redefine intrinsic {Word(word)}')
        runtime.register(word, definition.content)

class Add(Intrinsic):
    def __init__(self): super().__init__('+', 'a b -- a+b')
    def execute(self, runtime: Runtime) -> None:
        arg2, arg1 = runtime.pop_args([(Literal, Sequence), (Literal, Sequence)], True)
        if isinstance(arg1, Sequence):
            runtime.push(Sequence([*arg1.content, *arg2.content]))
        elif isinstance(arg1, NumberLiteral):
            runtime.push(NumberLiteral(arg1.value + arg2.value))
        elif isinstance(arg1, StringLiteral):
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

class Equal(Intrinsic):
    def __init__(self): super().__init__('=', 'a b -- a=b')
    def execute(self, runtime: Runtime) -> None:
        arg2, arg1 = runtime.pop_args([Literal, Literal])
        runtime.push(NumberLiteral(1 if arg1.value == arg2.value else 0))

class Different(Intrinsic):
    def __init__(self): super().__init__('<>', 'a b -- a<>b')
    def execute(self, runtime: Runtime) -> None:
        arg2, arg1 = runtime.pop_args([Literal, Literal])
        runtime.push(NumberLiteral(1 if arg1.value != arg2.value else 0))

class Lower(Intrinsic):
    def __init__(self): super().__init__('<', 'a b -- a<b')
    def execute(self, runtime: Runtime) -> None:
        arg2, arg1 = runtime.pop_args([Literal, Literal], True)
        runtime.push(NumberLiteral(1 if arg1.value < arg2.value else 0))

class LowerOrEqual(Intrinsic):
    def __init__(self): super().__init__('<=', 'a b -- a<=b')
    def execute(self, runtime: Runtime) -> None:
        arg2, arg1 = runtime.pop_args([Literal, Literal], True)
        runtime.push(NumberLiteral(1 if arg1.value <= arg2.value else 0))

class Greater(Intrinsic):
    def __init__(self): super().__init__('>', 'a b -- a>b')
    def execute(self, runtime: Runtime) -> None:
        arg2, arg1 = runtime.pop_args([Literal, Literal], True)
        runtime.push(NumberLiteral(1 if arg1.value > arg2.value else 0))

class GreaterOrEqual(Intrinsic):
    def __init__(self): super().__init__('>=', 'a b -- a>=b')
    def execute(self, runtime: Runtime) -> None:
        arg2, arg1 = runtime.pop_args([Literal, Literal], True)
        runtime.push(NumberLiteral(1 if arg1.value >= arg2.value else 0))

class Clear(Intrinsic):
    def __init__(self): super().__init__('clear', 'a1 .. an --')
    def execute(self, runtime: Runtime) -> None:
        runtime.stack = []

class Depth(Intrinsic):
    def __init__(self): super().__init__('depth', 'a1 .. an -- a1 .. an n')
    def execute(self, runtime: Runtime) -> None:
        runtime.push(NumberLiteral(len(runtime.stack)))

class Evaluate(Intrinsic):
    def __init__(self): super().__init__('!', 'a -- eval of a' )
    def execute(self, runtime: Runtime) -> None:
        arg = runtime.pop()
        if isinstance(arg, Quote):
            runtime.execute(arg.value)
        elif isinstance(arg, Sequence):
            for atom in arg.content: atom.execute(runtime)
        else: arg.execute(runtime)

class EvaluateIf(Intrinsic):
    def __init__(self): super().__init__('?!', 'a b -- eval of a if b')
    def execute(self, runtime: Runtime) -> None:
        cond = runtime.pop(NumberLiteral)
        if cond.value == 0: runtime.pop()
        else: runtime.execute('!')

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

class Bye(Intrinsic):
    def __init__(self): super().__init__('bye', 'interrupts execution')
    def execute(self, runtime: Runtime) -> None:
        runtime.halted = True

class Reverse(Intrinsic):
    def __init__(self): super().__init__('reverse', '{a1 .. an} -- {an .. a1}')
    def execute(self, runtime: Runtime) -> None:
        seq = runtime.pop(Sequence)
        runtime.push( Sequence(reversed(seq.content)) )

class Accept(Intrinsic):
    def __init__(self): super().__init__('accept', ' -- a ; user entered string')
    def execute(self, runtime: Runtime) -> None:
        runtime.push( StringLiteral(input('  ? ')) )

class Interpreter:
    ''' The interpreter program '''

    def __init__(self) -> None:
        os.system('')
        colorama.init(convert = True, strip = False)
        self.prompt = fg.LIGHTWHITE_EX + '> ' + fg.RESET
        self.showstack = True
        self.runtime = Runtime()
        for intrinsic in Intrinsic.all: intrinsic().register(self.runtime)
        self.parser = Parser()
        for pattern in Pattern.all: self.parser.register(pattern())
        for instruction in Interpreter.BOOTSTRAP: self.execute(instruction)
        print(f"Welcome to {fg.LIGHTWHITE_EX}FOLI{fg.RESET} {Comment('FOrth-Like Interpreter')}.")
        print(f'Type {Word("help")} for some hints about syntax and verbs.\n')

    def execute(self, expression: str) -> None:
        self.parser.execute(self.runtime, expression)

    BOOTSTRAP = [
        ':print (alias) .; :eval (alias) !;',
        ':stack (alias) .s; :help (alias) .w;',
        ':prepend (alias) <+; :append (alias) +>;',
        ':?if (alias ?!; :!= (alias) <>; : == (alias) =;',
        ':drop (a --) {} +> 0 * !;',
        ':dup (a -- a a) {} +> 2 * !;',
        ':swap (a b -- b a) {} +> +> !;',
        ':rot (a b c -- b c a) {} +> <+ +> !;',
        ':-rot (a b c -- c a b) rot rot;',
        ':nip (a b -- b) swap drop;',
        ':tuck (a b -- b a b) dup -rot;',
        ':over (a b -- a b a) swap tuck;',
        ':neg (a -- -a) 0 swap -;',
        ':?ifelse (a b c -- a ! if c | b ! otherwise) `swap` swap ?! swap drop !;',
        ':factorial (a -- a! , factorial of a)  dup 0 > if dup 1 - factorial * else drop 1 then;',
    ]

    def loop(self) -> None:
        while not self.runtime.halted:
            if self.showstack: self.execute('.s')
            try:
                self.execute(input(self.prompt))
            except Error as error:
                print(error)

# Main function calling
if __name__ == '__main__':
    Interpreter().loop()

