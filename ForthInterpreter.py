from typing import Iterable, Optional, Union, List, Mapping, Type
import os
import colorama
from colorama import Fore as fg


class Error(Exception):
    def __init__(self, msg) -> None:
        super().__init__(f'{fg.RED}ERROR:{fg.RESET} {msg}')


class Runtime:
    def __init__(self) -> None:
        self.dic: Mapping(str, Word) = {}
        self.stack: List[Token] = []

    def pop(self, type: Optional[Type['Token']] = None) -> 'Token':
        if len(self.stack) == 0:
            raise Error('empty stack')
        elem = self.stack.pop()
        if type is not None and not isinstance(elem, type):
            raise Error(f'not a {type.__name__.lower()} {elem}')
        return elem

    def args(self, types: List[Optional[Type]]) -> Iterable['Token']:
        n = len(types)
        if len(self.stack) < n:
            raise Error(f'not enough arguments on stack, need {n}')
        for i in range(n):
            yield self.pop(types[i])

    def push(self, token: 'Token') -> None:
        self.stack.append(token)

    def register(self, words: Union['Word', Iterable['Word']]):
        if isinstance(words, Word):
            self.register((words,))
        else:
            self.dic |= {word.name: word for word in words}


class Token:
    def __init__(self, runtime: Runtime):
        self.runtime = runtime

    def parse(self, input: Iterable['Token']) -> 'Token':
        return self

    def exec(self) -> None:
        pass

    def describe(self, definition: bool = False) -> str:
        return f'{self}'


class Comment(Token):
    def __init__(self, runtime: Runtime, value: Union[int, float, str]) -> None:
        super().__init__(runtime)
        self.value = value

    def __str__(self) -> str:
        return f'{fg.MAGENTA}( {self.value} ){fg.RESET}'


class Literal(Token):
    def __init__(self, runtime: Runtime, value: Union[int, float, str]) -> None:
        super().__init__(runtime)
        self.value = value

    def __str__(self) -> str:
        if isinstance(self.value, str):
            return f'{fg.GREEN}{self.value}{fg.RESET}'
        return f'{fg.CYAN}{self.value}{fg.RESET}'

    @staticmethod
    def is_literal(token: Token, value_type: Optional[type]) -> bool:
        return isinstance(token, Literal) and (
            value_type is None or isinstance(token.value, value_type)
        )

    def exec(self) -> None:
        self.runtime.push(self)


class Word(Token):
    def __init__(self, runtime: Runtime, name: Optional[str] = None) -> None:
        super().__init__(runtime)
        self.name = name.lower() if name is not None else None

    def __str__(self) -> str:
        return fg.YELLOW + self.name + fg.RESET

    def describe(self, definition: bool = False) -> str:
        return (
            f'intrinsic<{fg.LIGHTBLACK_EX}{type(self).__name__}{fg.RESET}>'
            if definition or self.name is None
            else f'{self}'
        )

    def parse_many(self, input: Iterable[Token], until: Optional[str] = None) -> Iterable[Token]:
        while True:
            token = next(input, None)
            if token is None:
                if until is None:
                    break
                else:
                    raise Error(f'missing {until} to close word')
            if isinstance(token, Word) and token.name == until:
                break
            yield token.parse(input)


class Reference(Word):
    def deref(self) -> Word:
        if not self.name in self.runtime.dic:
            raise Error(f'unknown word {self}')
        return self.runtime.dic[self.name]

    def parse(self, input: Iterable[Token]) -> Token:
        if not self.name in self.runtime.dic:
            return self
        return self.deref().parse(input)

    def __str__(self) -> str:
        return fg.YELLOW + '`' + self.name + '`' + fg.RESET

    def exec(self):
        return self.deref().exec()


class Container(Word):
    def __init__(
        self,
        runtime: Runtime,
        name: Optional[str] = None,
        content: Optional[List[Token]] = None,
    ) -> None:
        super().__init__(runtime, name)
        self.content = content

    def describe(self, definition: bool = False) -> str:
        if not definition and self.name is not None:
            return super().describe()
        if self.content is None:
            return 'undefined'
        descr = ' '.join(token.describe() for token in self.content)
        return descr if definition else '{' + descr + '}'

    def parse(self, input: Iterable[Token], until: Optional[str] = None) -> Token:
        if self.content is None:
            self.content = [*self.parse_many(input, until)]
        return self

    def exec(self):
        for token in self.content:
            token.exec()


class Boxed(Word):
    def __init__(self, word: Word) -> None:
        super().__init__(word.runtime, None)
        self.word = word

    def describe(self, definition: bool = False) -> str:
        return self.word.describe(definition)

    def exec(self):
        self.runtime.push(self.word)


class Print(Word):
    def exec(self) -> None:
        print(f'  = {self.runtime.pop().describe()}')


class PrintStack(Word):
    def exec(self) -> None:
        print('STACK')
        i = len(self.runtime.stack)
        if i == 0:
            print('  Empty')
            return
        for token in self.runtime.stack:
            print(f'  {i} : {token.describe()}')
            i -= 1


class PrintWords(Word):
    def exec(self) -> None:
        print('WORDS')
        # intrinsics = [f'{word}' for word in self.runtime.dic.values() if not isinstance(word, Container)]
        # print('  intrinsics : ' + ' , '.join(intrinsics))
        # for _, word in self.runtime.dic.items():
        # 	if isinstance(word, Container):
        # 		print(f'  {word} = {word.describe(True)}')
        for _, word in self.runtime.dic.items():
            print(f': {word}   {word.describe(True)} ;')


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


class Enclosed(Word):
    def __init__(self, runtime: Runtime, name: str, closure: str) -> None:
        super().__init__(runtime, name)
        self.closure = closure

    def __str__(self) -> str:
        return fg.YELLOW + self.name + fg.RESET + '..' + fg.YELLOW + self.closure + fg.RESET


class Definition(Enclosed):
    def parse(self, input: Iterable[Token]) -> Token:
        word = next(input, None)
        if word is None:
            raise Error('word name missing in definition')
        return Container(
            self.runtime,
            None,
            [
                Boxed(Container(self.runtime).parse(input, self.closure)),
                Boxed(word),
                self.runtime.dic['def'],
            ],
        )

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


class Sequence(Enclosed):
    def parse(self, input: Iterable[Token]) -> Token:
        return Boxed(Container(self.runtime).parse(input, self.closure))


class Quote(Enclosed):
    def parse(self, input: Iterable[Token]) -> Token:
        ref = next(input, None)
        if not isinstance(ref, Reference) or ref.name == self.closure:
            raise Error(f'invalid quote content {ref}')
        end = next(input, None)
        if not isinstance(end, Word) or end.name != self.closure:
            raise Error(f'missing {self.closure} to close word')
        return Boxed(ref)


class Define(Word):
    def exec(self) -> None:
        key, value = self.runtime.args([Reference, None])
        name = key.name
        if name in self.runtime.dic and not isinstance(self.runtime.dic[name], Container):
            raise Error(f'cannot redefine intrinsic word {name}')
        if isinstance(value, Container):
            self.runtime.register(Container(self.runtime, name, value.content))
        else:
            self.runtime.register(Container(self.runtime, name, [value]))


class Execute(Word):
    def exec(self) -> None:
        self.runtime.pop().exec()


class ExecuteIf(Word):
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


class InputParser:
    def __init__(self, runtime: Runtime):
        self.runtime = runtime

    @staticmethod
    def is_separator(c: str) -> bool:
        return c.isspace() or c in (':', ';', '{', '}', '`', "'", '"', '(', ')')

    @staticmethod
    def find_separator(s: str) -> int:
        for i in range(len(s)):
            if InputParser.is_separator(s[i]):
                return i
        return -1

    def parse(self, input: str) -> Iterable[Token]:
        while True:
            input = input.lstrip()
            if len(input) == 0:
                break
            if input[0] in ("'", '"'):
                # string literal
                idx = input.find(input[0], 1)
                if idx < 0:
                    raise Error(f'string not closed {input}')
                yield Literal(self.runtime, input[1:idx])
                input = input[idx + 1 :]
            elif input[0] == '(':
                # comment
                idx = input.find(')', 1)
                if idx < 0:
                    raise Error(f'comment not closed {input}')
                yield Comment(self.runtime, input[1:idx])
                input = input[idx + 1 :]
            else:
                # int, float literals or word reference
                idx = InputParser.find_separator(input)
                if idx < 0:
                    token = input
                    input = ''
                elif idx == 0:
                    token = input[0]
                    input = input[1:]
                else:
                    token = input[:idx]
                    input = input[idx:]
                try:
                    yield Literal(self.runtime, int(token))
                except:
                    try:
                        yield Literal(self.runtime, float(token))
                    except:
                        yield Reference(self.runtime, token)

    def exec(self, input: str) -> None:
        Container(self.runtime).parse(self.parse(input)).exec()


def main() -> None:
    os.system('')
    colorama.init(convert=True, strip=False)
    prompt = fg.LIGHTWHITE_EX + '> ' + fg.RESET
    showstack = True

    runtime = Runtime()
    parser = InputParser(runtime)

    # register intrinsics
    runtime.register(
        [
            Print(runtime, '.'),
            PrintStack(runtime, '.s'),
            PrintWords(runtime, '.w'),
            Execute(runtime, '!'),
            ExecuteIf(runtime, '?!'),
            Definition(runtime, ':', ';'),
            Define(runtime, 'def'),
            Sequence(runtime, '{', '}'),
            Quote(runtime, '`', '`'),
            Reverse(runtime, 'rev'),
            Prepend(runtime, '<+'),
            Postpend(runtime, '+>'),
            Depth(runtime, 'depth'),
            ArithmeticOp(runtime, '+'),
            ArithmeticOp(runtime, '-'),
            ArithmeticOp(runtime, '*'),
            ArithmeticOp(runtime, '/'),
            IfStatement(runtime, 'if', 'then'),
        ]
    )
    # registers functions
    parser.exec(':drop (a --) {} +> 0 * !;')
    parser.exec(':dup (a -- a a) {} +> 2 * !;')
    parser.exec(':swap (a b -- b a) {} +> +> !;')
    parser.exec(':rot (a b c -- b c a) {} <+ <+ +> !;')
    parser.exec(':-rot (a b c -- c a b) rot rot;')
    parser.exec(':nip (a b -- b) swap drop;')
    parser.exec(':tuck (a b -- b a b) dup -rot;')
    parser.exec(':over (a b -- a b a) swap tuck;')
    parser.exec(':?ifel (a b c -- a ! | b !) {swap} swap ?! swap drop !;')
    parser.exec(':facto (a -- a!)  dup {dup 1 - facto *} {drop 1} rot ?ifel ;')

    while True:
        if showstack:
            parser.exec('.s')
        try:
            parser.exec(input(prompt))
        except Error as e:
            print(e)
        except Exception as e:
            print(e)


main()
