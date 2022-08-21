from typing import Dict, Set, Iterable, Optional, Tuple, Union, Type, TYPE_CHECKING
from colorama import Fore as fg
from atoms import Error, Atom, Literal, NumberLiteral, StringLiteral, Comment, Word
if TYPE_CHECKING: from execution import Runtime

class Keyword(Atom):
    ''' Intermediate output of the Tokenizer. Not a proper language element. '''
    def __init__(self, value: str) -> None:
        self.value = value
    def __str__(self) -> str:
        return f'{fg.MAGENTA}<{self.value}>{fg.RESET}'

class ParsingError(Error):
    ''' Raised by the parser. '''

class ParsingIncomplete(ParsingError):
    ''' Raised by the parser when end of line is reached prematuretly. '''

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
        return c.isspace() or c in (':', ';', '{', '}', "'", '"', '(', ')', '`')

    @staticmethod
    def is_number(s: str) -> bool:
        try: _ = int(s); return True
        except ValueError:
            try: _ = float(s) ; return True
            except ValueError: return False

    def parse_string(self, prefix: str, suffix: str) -> Optional[str]:
        if self.input[0] != prefix: return None
        idx = self.input.find(suffix, 1)
        if idx < 0: raise ParsingIncomplete(f'missing closing {suffix}')
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
    It has the form : prefix ... suffix. Parsing implementation is provided in sub classes.
    '''
    classes : Set[Type['Pattern']] = set()
    def __init_subclass__(cls) -> None: Pattern.classes.add(cls)
    def __init__(self, prefix: str = '', suffix: str = '', comment: Optional[str] = None) -> None:
        self.prefix = prefix ; self.suffix = suffix ; self.comment = comment
    def parse(self, _parser: 'Parser') -> Iterable[Atom]:
        ... # to overload
    def reserved(self) -> Iterable[str]:
        yield self.prefix
        if self.suffix is not None: yield self.suffix
    def __str__(self) -> str:
        return f'{self.prefix}{fg.LIGHTBLACK_EX} .. {fg.RESET}{self.suffix}'
    def register(self, parser: 'Parser'):
        parser.register(self.prefix, self)
    def describe(self) -> str:
        desc = f'{Comment(self.comment)} ' if self.comment is not None else ''
        desc +=  f'{fg.LIGHTBLACK_EX}pattern<{type(self).__name__}>{fg.RESET}'
        return desc

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

    def register(self, prefix: str, pattern: Pattern) -> None:
        self.patterns[prefix] = pattern

    def parse(self, input_str: str) -> Iterable[Atom]:
        self.input = Tokenizer(input_str).tokenize()
        yield from self.parse_many()

    def next(self) -> Optional[Token]:
        if self.input is None: return None
        return next(iter(self.input), None)

    def parse_many(self, closure: Tuple[Optional[str],...] = (None, ), ignore: Tuple[str,...] = ()) -> Iterable[Atom]:
        self.closure = None
        while True:
            token = self.next()
            if token is None and None in closure: break
            if token is None: raise ParsingIncomplete(f'missing closing {closure}')
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
            raise ParsingError(f'word name {word} contains an invalid character')
        if word in self.reserved_patterns():
            raise ParsingError(f'word name {word} is reserved')
        if Tokenizer.is_number(word):
            raise ParsingError(f'word name {word} is a number')

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
