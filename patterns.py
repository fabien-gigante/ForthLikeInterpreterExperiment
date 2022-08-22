''' All patterns implementations '''

from typing import Iterable, List
from colorama import Fore as fg
from atoms import Atom, Word, Sequence
from parsing import Pattern, NumberLiteral, Parser, ParsingError, ParsingIncomplete

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
        word = parser.parse_word()
        ((atoms, _),) = parser.parse_pattern((self.suffix,))
        yield from [Sequence(atoms), Sequence([word]), Word('def')]

class VariablePattern(Pattern):
    ''' Pattern -> ...  to define new variables. '''
    def __init__(self) : super().__init__('->', None, 'introduces variables')
    def parse(self, parser: Parser) -> Iterable[Atom]:
        content : List[Atom] = []
        ((variables, _),) = parser.parse_pattern(('{',))
        if len(variables) == 0: raise ParsingError('missing variables after ->')
        for var in variables:
            if not isinstance(var, Word): raise ParsingError(f'invalid variable type {var}')
            content = [Sequence([var]), Word('var')] + content
        ((expression, _),) = parser.parse_pattern(('}',))
        content += expression
        yield Sequence(content)
        yield Word('eval')
    def __str__(self) -> str:
        return self.prefix + Pattern.ELLIPSIS + '{' + Pattern.ELLIPSIS + '}'

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
        return self.prefix + Pattern.ELLIPSIS + '[else' + Pattern.ELLIPSIS + '] ' + self.suffix

class BeginPattern(Pattern):
    '''
    Pattern begin ... again, for forever loops 
    Patterns begin ... until, begin ... while ... repeat, for conditional loops 
    '''
    def __init__(self) :
        super().__init__('begin', 'again', 'loop with optional condition')
    def parse(self, parser: Parser) -> Iterable[Atom]:
        content = []
        for atoms, key in parser.parse_pattern((self.suffix, 'until', 'repeat'), ('while',)):
            content += atoms
            if key == 'until': content += [ Word('?leave') ]
            if key == 'while': content += [ Word('not'), Word('?leave') ]
        yield Sequence(content)
        yield Word('forever')
    def reserved(self) -> Iterable[str]:
        yield from super().reserved()
        yield 'until' ; yield 'while' ; yield 'repeat'
    def __str__(self) -> str:
        return super().__str__() + ' | until | while' + Pattern.ELLIPSIS + 'repeat'

class ForPattern(Pattern):
    ''' Pattern for .. next | step, for indexed loops. '''
    def __init__(self) :
        super().__init__('for', 'next', 'indexed loop')
    def parse(self, parser: Parser) -> Iterable[Atom]:
        index = parser.parse_word()
        end = Word(index.value + '.end')
        ((atoms, closure),) = parser.parse_pattern((self.suffix, 'step'))
        if closure == 'step': (*atoms, increment) = atoms
        else: increment = NumberLiteral(1)
        atoms += [ index, end, Word('='), Word('?leave')]
        atoms += [index, increment, Word('+'), Sequence([index]), Word('sto') ]
        yield Sequence([Sequence([end]), Word('var'), Sequence([index]), Word('var'), Sequence(atoms), Word('forever')])
        yield Word('eval')

class Quote(Pattern):
    ''' Pattern quote .. '''
    def __init__(self) :
        super().__init__('quote', None, 'quoting a variable')
    def parse(self, parser: Parser) -> Iterable[Atom]:
        yield Sequence([parser.parse_word()])
        
