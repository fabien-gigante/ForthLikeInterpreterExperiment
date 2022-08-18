from typing import Iterable, Optional, Union, List, Mapping, Type
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
    ''' Atomic int, float or string literal value. '''
    def __str__(self) -> str:
        if isinstance(self.value, str):
            return f"{fg.CYAN}'{self.value}'{fg.RESET}"
        else:
            return f'{fg.CYAN}{self.value}{fg.RESET}'

    def execute(self, runtime: 'Runtime') -> None:
        runtime.push(self)

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
    def register(self, runtime: 'Runtime'): 
        runtime.register(self.value, [self])
    def __str__(self) -> str:
        return f'{fg.LIGHTBLACK_EX}intrinsic<{type(self).__name__}>{fg.RESET}'

class Tokenizer:
    ''' 
    Syntaxical tokenizer. 
    Turns a string into a series of tokens. 
    Can only produce Litteral, Comment, Quote or Keyword.
    '''

    def __init__(self, input: str) -> None:
        self.input = input

    @staticmethod
    def is_separator(c: str) -> bool:
        return c.isspace() or c in (':', ';', '{', '}', '`', "'", '"', '(', ')', '`')

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
            if token is not None: yield Literal(token); continue
            token = self.parse_string("'", "'")
            if token is not None: yield Literal(token); continue
            token = self.parse_string('(', ')')
            if token is not None: yield Comment(token); continue
            token = self.parse_string('`', '`')
            if token is not None: yield Quote(token); continue
            token = self.parse_until_separator()
            try: yield Literal(int(token))
            except:
                try: yield Literal(float(token))
                except: yield Keyword(token.lower())

class Parser:
    ''' 
    Gramatical parser. 
    Parses a string or a series of tokens. 
    Resolves any Keyword into Word or Sequence. 
    Can therefore only produce Litteral, Comment, Quote or Word or Sequence.
    '''

    def parse(self, input: str) -> Iterable[Atom]:
        self.input = Tokenizer(input).tokenize()
        yield from self.parse_many()

    def parse_many(self, closure: Optional[str] = None) -> Iterable[Atom]:
        while True:
            token = next(self.input, None)
            if token is None and closure is None: break
            if token is None: raise Error(f'missing closing {closure}')
            if isinstance(token, Keyword) and token.value == closure: break
            yield from self.parse_one(token)

    def is_valid_word(self, word: str) -> bool:
        return not any(Tokenizer.is_separator(ch) for ch in word)

    def parse_one(self, token: Token) -> Iterable[Atom]:
        if not isinstance(token, Keyword): 
            yield token ; return
        if token.value == '{': 
            yield Sequence(self.parse_many('}'))
        elif token.value == ':': 
            word = next(self.input, None)
            if word is None: raise Error(f'missing word in definition')
            if not isinstance(word, Keyword) or not self.is_valid_word(word.value):
               raise Error(f'invalid word in definition')
            yield Sequence(self.parse_many(';'))
            yield Quote(word.value)
            yield Word('def')
        elif not self.is_valid_word(token.value):
            raise Error(f'invalid character in word {token.value}')
        else: yield Word(token.value)

    def execute(self, runtime: 'Runtime', input: str) -> None:
        atoms = self.parse(input)
        for atom in atoms: atom.execute(runtime)

class Runtime:
    '''
    Runtime environement for execution.
    Holds and manages the list of available words and the stack.
    '''

    def __init__(self) -> None:
        self.words: Mapping(str, List[Atom]) = {}
        self.stack: List[Atom] = []

    def pop(self, type: Optional[Type[Atom]] = None) -> Atom:
        if len(self.stack) == 0:
            raise Error('empty stack')
        elem = self.stack.pop()
        if type is not None and not isinstance(elem, type):
            raise Error(f'not a {type.__name__.lower()} {elem}')
        return elem

    def args(self, types: List[Optional[Type]]) -> Iterable[Atom]:
        n = len(types)
        if len(self.stack) < n:
            raise Error(f'not enough arguments on stack, need {n}')
        for i in range(n):
            yield self.pop(types[i])

    def push(self, atom: Atom) -> None:
        self.stack.append(atom)

    def is_intrinsic(self, word: str) -> bool:
        if not word in self.words: return False
        return all(isinstance(atom, Intrinsic) for atom in self.words[word])

    def register(self, word: str, definition: List[Atom]) -> None:
        self.words[word] = [*definition]

    def execute(self, word: str) -> None:
        if not word in self.words: raise Error(f'unknown word {word}')
        for atom in self.words[word]: atom.execute(self)

    def describe(self, word: str) -> str:
        return ' '.join( f'{atom}' for atom in self.words[word] )

'''
    All intrinsic implementations
'''

class Print(Intrinsic):
    def __init__(self): super().__init__('.')
    def execute(self, runtime: Runtime) -> None:
        print(f'  = {runtime.pop()}')

class PrintWords(Intrinsic):
    def __init__(self): super().__init__('.w')
    def execute(self, runtime: Runtime) -> None:
        for key in sorted(runtime.words.keys()):
            print(f'  : {fg.YELLOW}{key}{fg.RESET} {runtime.describe(key)} ;')

class PrintStack(Intrinsic):
    def __init__(self): super().__init__('.s')
    def execute(self, runtime: Runtime) -> None:
        i = len(runtime.stack)
        for atom in runtime.stack:
            print(f'  {i} : {atom}')
            i -= 1

class Define(Intrinsic):
    def __init__(self): super().__init__('def')
    def execute(self, runtime: Runtime) -> None:
        quote, definition = runtime.args([Quote, Sequence])
        word = quote.value
        if any(Tokenizer.is_separator(ch) for ch in word):
           raise Error(f'invalid character in word {word}')
        if runtime.is_intrinsic(word):
           raise Error(f'cannot redefine intrinsic {word}')
        runtime.register(word, definition.content)

class Add(Intrinsic):
    def __init__(self): super().__init__('+')
    def execute(self, runtime: Runtime) -> None:
        arg2, arg1 = runtime.args([None, None])
        if isinstance(arg1, Literal) and isinstance(arg2, Literal):
            runtime.push(Literal(arg1.value + arg2.value))
        elif isinstance(arg1, Sequence) and isinstance(arg2, Sequence):
            runtime.push(Sequence([*arg1.content, *arg2.content]))
        else: raise Error(f'Invalid argument types for {self.value}');

class Substract(Intrinsic):
    def __init__(self): super().__init__('-')
    def execute(self, runtime: Runtime) -> None:
        arg2, arg1 = runtime.args([Literal, Literal])
        runtime.push(Literal(arg1.value - arg2.value))

class Multiply(Intrinsic):
    def __init__(self): super().__init__('*')
    def execute(self, runtime: Runtime) -> None:
        arg2, arg1 = runtime.args([None, None])
        if isinstance(arg1, Literal) and isinstance(arg2, Literal):
            runtime.push(Literal(arg1.value * arg2.value))
        elif isinstance(arg1, Sequence) and isinstance(arg2, Literal) and isinstance(arg2.value, int):
            runtime.push(Sequence(arg1.content * arg2.value))
        elif isinstance(arg2, Sequence) and isinstance(arg1, Literal) and isinstance(arg1.value, int):
            runtime.push(Sequence(arg1.value * arg2.content))
        else: raise Error(f'Invalid argument types for {self.value}');

class Divide(Intrinsic):
    def __init__(self): super().__init__('/')
    def execute(self, runtime: Runtime) -> None:
        arg2, arg1 = runtime.args([Literal, Literal])
        runtime.push(Literal(arg1.value / arg2.value))

class Clear(Intrinsic):
    def __init__(self): super().__init__('clear')
    def execute(self, runtime: Runtime) -> None:
        runtime.stack = []

class Depth(Intrinsic):
    def __init__(self): super().__init__('depth')
    def execute(self, runtime: Runtime) -> None:
        runtime.push(Literal(len(runtime.stack)))

class Evaluate(Intrinsic):
    def __init__(self): super().__init__('!')
    def execute(self, runtime: Runtime) -> None:
        arg = runtime.pop()
        if isinstance(arg, Quote):
            runtime.execute(arg.value)
        elif isinstance(arg, Sequence):
            for atom in arg.content: atom.execute(runtime)
        else: arg.execute(runtime)

class Prepend(Intrinsic):
    def __init__(self): super().__init__('<+')
    def execute(self, runtime: Runtime) -> None:
        seq, atom = runtime.args([Sequence, None])
        runtime.push( Sequence([atom, *seq.content]) )

class Postpend(Intrinsic):
    def __init__(self): super().__init__('+>')
    def execute(self, runtime: Runtime) -> None:
        seq, atom = runtime.args([Sequence, None])
        runtime.push( Sequence([*seq.content, atom]) )

# Interpreter main loop
def main() -> None:
    os.system('') ; colorama.init(convert = True, strip = False)
    prompt = fg.LIGHTWHITE_EX + '> ' + fg.RESET
    showstack = True

    runtime = Runtime()
    for intrinsic in Intrinsic.all: intrinsic().register(runtime)

    parser = Parser()
    parser.execute(runtime, ':print ( alias ) .; :eval ( alias ) !; :stack ( alias ) .s; :words ( alias ) .w; :prepend ( alias ) <+; :postpend ( alias ) +>;')

    while True:
        if showstack: parser.execute(runtime, '.s')
        try:
            parser.execute(runtime, input(prompt))
        except Error as e:
            print(e)
        #except Exception as e:
        #    print(e)

# Main function calling
if __name__ == '__main__':
    main()

'''

class IfStatement(Enclosed):
    def parse(self, input: Iterable[Token]) -> Token:
        then_content = [*self.parse_many(input, self.closure)]
        else_content = None
        for i in range(len(then_content)):
            if isinstance(then_content[i], Word) and then_content[i].name == 'else':
                (then_content, else_content) = (then_content[:i], then_content[i+1:])
                break
        if else_content is None:
            return Container(
                self.runtime,
                None,
                [
                    Boxed(Container(self.runtime,None,then_content)),
                    self.runtime.dic['swap'],
                    self.runtime.dic['?!'],
                ],
            )
        else:
            return Container(
                self.runtime,
                None,
                [
                    Boxed(Container(self.runtime,None,then_content)),
                    Boxed(Container(self.runtime,None,else_content)),
                    self.runtime.dic['rot'],
                    self.runtime.dic['?ifel'],
                ],
            )


class EvalIf(Word):
    def exec(self) -> None:
        cond, statement = self.runtime.args([Literal, None])
        if cond.value != 0:
            statement.exec()

class Reverse(Word):
    def exec(self) -> None:
        container = self.runtime.pop(Container)
        self.runtime.push(Container(self.runtime, None, [*reversed(container.content)]))

'''
