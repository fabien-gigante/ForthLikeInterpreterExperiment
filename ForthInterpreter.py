from pickle import FALSE
from typing import Iterable, Optional, Union, List, Mapping, Type
import os
import colorama
from colorama import Fore as fg


class Error(Exception):
    ''' Applicative error '''
    def __init__(self, msg) -> None:
        super().__init__(f'{fg.RED}ERROR:{fg.RESET} {msg}')

class Atom:
    def execute(self, runtime: 'Runtime') -> None:
        raise Error(f'atom {self} cannot be executed')

class Token(Atom):
    def __init__(self, value: Union[int,float,str]) -> None:
        self.value = value

class Literal(Token):
    def __str__(self) -> str:
        if isinstance(self.value, str):
            return f"{fg.CYAN}'{self.value}'{fg.RESET}"
        else:
            return f'{fg.CYAN}{self.value}{fg.RESET}'

    def execute(self, runtime: 'Runtime') -> None:
        runtime.push(self)

class Comment(Token):
    def __str__(self) -> str:
        return f'{fg.GREEN}({self.value}){fg.RESET}'

    def execute(self, runtime: 'Runtime') -> None:
        pass

class Keyword(Token):
    def __str__(self) -> str:
        return f'{fg.MAGENTA}<{self.value}>{fg.RESET}'

class Quote(Token):
    def __str__(self) -> str:
        return f'{fg.YELLOW}`{self.value}`{fg.RESET}'
    def execute(self, runtime: 'Runtime') -> None:
        runtime.push(self)

class Tokenizer:
    ''' Syntaxical analysis : turns a string into a series of tokens '''

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
    ''' Gramatical analysis : parses a series of tokens, resolves keywords into words or sequences '''

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

    def parse_one(self, token: Token) -> Iterable[Atom]:
        if not isinstance(token, Keyword): 
            yield token ; return
        if token.value == '{': 
            yield Sequence(self.parse_many('}'))
        elif token.value == ':': 
            word = next(self.input, None)
            if word is None: raise Error(f'missing word in definition')
            if not isinstance(word, Keyword): raise Error(f'invalid word in definition')
            yield Sequence(self.parse_many(';'))
            yield Quote(word.value)
            yield Word('def')
        elif any(Tokenizer.is_separator(ch) for ch in token.value):
            raise Error(f'invalid character in word {token.value}')
        else: yield Word(token.value)

    def execute(self, runtime: 'Runtime', input: str) -> None:
        atoms = self.parse(input)
        for atom in atoms: atom.execute(runtime)

class Runtime:
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
        if len(self.words[word]) != 1: return False
        intrinsic = self.words[word][0]
        return isinstance(self.words[word][0], Intrinsic) and intrinsic.value == word

    def register(self, word: str, definition: List[Atom]) -> None:
        self.words[word] = [*definition]

    def execute(self, word: str) -> None:
        if not word in self.words: raise Error(f'unknown word {word}')
        for atom in self.words[word]: atom.execute(self)

    def describe(self, word: str) -> str:
        return ' '.join( f'{atom}' for atom in self.words[word] )

class Word(Token):
    def __str__(self) -> str:
        return f'{fg.YELLOW}{self.value}{fg.RESET}'
    def execute(self, runtime: 'Runtime') -> None:
        runtime.execute(self.value)

class Sequence(Atom):
    def __init__(self, content: Iterable[Atom]):
        super().__init__()
        self.content = [*content]
    def __str__(self) -> str:
        return '{ ' + ' '.join( f'{atom}' for atom in self.content ) + ' }'
    def execute(self, runtime: 'Runtime') -> None:
        runtime.push(self)

class Intrinsic(Token):
    def register(self, runtime: Runtime) : runtime.register(self.value, [self])
    def __str__(self) -> str:
        return f'{fg.LIGHTBLACK_EX}intrinsic<{type(self).__name__}>{fg.RESET}'

class Print(Intrinsic):
    def __init__(self): super().__init__('.')
    def execute(self, runtime: Runtime) -> None:
        print(f'  = {runtime.pop()}')

class PrintWords(Intrinsic):
    def __init__(self): super().__init__('.w')
    def execute(self, runtime: Runtime) -> None:
        for key in runtime.words.keys():
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

def main() -> None:
    os.system('')
    colorama.init(convert=True, strip=False)
    prompt = fg.LIGHTWHITE_EX + '> ' + fg.RESET
    showstack = True

    runtime = Runtime()
    for intrinsic in [Define, Print, PrintStack, PrintWords]:
        intrinsic().register(runtime)

    parser = Parser()
    while True:
        if showstack: parser.execute(runtime, '.s')
        try:
            parser.execute(runtime, input(prompt))
        except Error as e:
            print(e)
        #except Exception as e:
        #    print(e)

main()

'''

class ArithmeticOp(Word):
    def compute(self, value1, value2):
        if self.name == '+':
            return value1 + value2
        if self.name == '-':
            return value1 - value2
        if self.name == '*':
            return value1 * value2
        if self.name == '/':
            return value1 / value2
        return None

    def concat(self, arg1, arg2):
        if isinstance(arg1, Container) and isinstance(arg2, Container):
            return Container(self.runtime, None, arg1.content + arg2.content)
        return None

    def copies(self, arg1, arg2):
        if isinstance(arg1, Container) and Literal.is_literal(arg2, int):
            return Container(self.runtime, None, arg1.content * arg2.value)
        return None

    def exec(self) -> None:
        result = None
        (arg2, arg1) = self.runtime.args([None, None])
        if isinstance(arg1, Literal) and isinstance(arg2, Literal):
            result = Literal(self.runtime, self.compute(arg1.value, arg2.value))
        if isinstance(arg1, Container) or isinstance(arg2, Container):
            if self.name == '+':
                result = self.concat(arg1, arg2)
            elif self.name == '*':
                result = self.copies(arg1, arg2)
        if result is None:
            raise Error(f'Invalid arguments for operator {self.name} {arg1} {arg2}')
        self.runtime.push(result)


class Prepend(Word):
    def exec(self) -> None:
        result = None
        (arg2, arg1) = self.runtime.args([Container, None])
        result = Container(self.runtime, None, [Boxed(arg1)] + arg2.content)
        self.runtime.push(result)


class Postpend(Word):
    def exec(self) -> None:
        result = None
        (arg2, arg1) = self.runtime.args([Container, None])
        result = Container(self.runtime, None, arg2.content + [Boxed(arg1)])
        self.runtime.push(result)

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


class Eval(Word):
    def exec(self) -> None:
        self.runtime.pop().exec()


class EvalIf(Word):
    def exec(self) -> None:
        cond, statement = self.runtime.args([Literal, None])
        if cond.value != 0:
            statement.exec()

class Reverse(Word):
    def exec(self) -> None:
        container = self.runtime.pop(Container)
        self.runtime.push(Container(self.runtime, None, [*reversed(container.content)]))

class Depth(Word):
    def exec(self) -> None:
        self.runtime.push(Literal(self.runtime, len(self.runtime.stack)))

'''