''' All patterns implementations '''

from typing import Iterable, List
from colorama import Fore as fg
from atoms import Atom, Word, Sequence
from parsing import Pattern, Keyword, Parser, ParsingError, ParsingIncomplete

class SequencePattern(Pattern):
    ''' Pattern { ... }  to define a sequence. '''
    def __init__(self) : super().__init__('{', '}','builds a sequence')
    def parse(self, parser: Parser) -> Iterable[Atom]:
        ((atoms, _),) = parser.parse_pattern((self.suffix,))
        yield Sequence(atoms)

class ExpressionPattern(Pattern):
    ''' Alternative pattern ` ... `  to define a sequence. Alias for { ... }. '''
    def __init__(self) : super().__init__('`', '`', 'alias of { ... }')
    def parse(self, parser: Parser) -> Iterable[Atom]:
        ((atoms, _),) = parser.parse_pattern((self.suffix,))
        yield Sequence(atoms)

class DefinePattern(Pattern):
    ''' Pattern : ... ;  to define a new word. '''
    def __init__(self) : super().__init__(':', ';', 'defines a new word')
    def parse(self, parser: Parser) -> Iterable[Atom]:
        word = parser.next()
        if word is None: raise ParsingIncomplete('missing word in definition')
        if not isinstance(word, Keyword): raise ParsingError(f'invalid word type {word}')
        parser.check_valid_word(word.value)
        ((atoms, _),) = parser.parse_pattern((self.suffix,))
        yield Sequence(atoms)
        yield Sequence([Word(word.value)])
        yield Word('def')

class VariablePattern(Pattern):
    ''' Pattern -> ...  to define new variables. '''
    def __init__(self) : super().__init__('->', None, 'introduces variables')
    def parse(self, parser: Parser) -> Iterable[Atom]:
        content : List[Atom] = []
        ((variables, _),) = parser.parse_pattern(('{',))
        if len(variables) == 0: raise ParsingError('missing variables after ->')
        for var in variables:
            if not isinstance(var, Word): raise ParsingError(f'invalid variable type {var}')
            content += [Sequence([var]), Word('var')]
        ((expression, _),) = parser.parse_pattern(('}',))
        content += expression
        yield Sequence(content)
        yield Word('eval')
    def __str__(self) -> str:
        return f'{self.prefix}{fg.LIGHTBLACK_EX} .. {fg.RESET}' + ' { ' + f'{fg.LIGHTBLACK_EX} .. {fg.RESET}' + ' }'

class IfPattern(Pattern):
    ''' Pattern if ... else ... then, used to define conditional logic. '''
    def __init__(self) :
        super().__init__('if', 'then', 'conditional logic')
    def parse(self, parser: Parser) -> Iterable[Atom]:
        expr_true = None ; expr_false = None
        for atoms, sep in parser.parse_pattern((self.suffix,), ('else',)):
            if sep == 'else' and expr_true is not None:
                raise ParsingError('A single else keyword is allowed in an if statement')
            if sep == 'then' and expr_true is not None: expr_false = atoms
            else: expr_true = atoms
        if expr_false is None:
            yield from (Sequence(expr_true), Word('swap'), Word('?if'))
        else:
            yield from (Sequence(expr_true), Sequence(expr_false), Word('rot'), Word('?ifelse'))
    def reserved(self) -> Iterable[str]:
        yield from super().reserved()
        yield 'else'
    def __str__(self) -> str:
        return f'{self.prefix}{fg.LIGHTBLACK_EX} .. {fg.RESET}[else {fg.LIGHTBLACK_EX}..{fg.RESET}] {self.suffix}'

class BeginPattern(Pattern):
    ''' Pattern begin ... again, for forever loops. '''
    def __init__(self) :
        super().__init__('begin', 'again', 'loop with optional condition')
    def parse(self, parser: Parser) -> Iterable[Atom]:
        content = []
        for atoms, sep in parser.parse_pattern((self.suffix, 'until', 'repeat'), ('while',)):
            content += atoms
            if sep == 'until': content += [ Word('?leave') ]
            if sep == 'while': content += [ Word('not'), Word('?leave') ]
        yield Sequence(content)
        yield Word('forever')
    def reserved(self) -> Iterable[str]:
        yield from super().reserved()
        yield 'until' ; yield 'while' ; yield 'repeat'
    def __str__(self) -> str:
        return super().__str__() + f' | until | while {fg.LIGHTBLACK_EX}..{fg.RESET} repeat'

