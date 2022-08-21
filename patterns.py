from typing import Iterable
from colorama import Fore as fg
from atoms import Atom, Word, Sequence
from parsing import Pattern, Keyword, Parser, ParsingError, ParsingIncomplete

class SequencePattern(Pattern):
    ''' Pattern { ... }  to define a sequence. '''
    def __init__(self) : super().__init__('{', '}','builds a sequence')
    def parse(self, parser: Parser) -> Iterable[Atom]:
        yield Sequence(parser.parse_many((self.suffix,)))

class ExpressionPattern(Pattern):
    ''' Alternative pattern ` ... `  to define a sequence. Alias for { ... }. '''
    def __init__(self) : super().__init__('`', '`', 'alias of { ... }')
    def parse(self, parser: Parser) -> Iterable[Atom]:
        yield Sequence(parser.parse_many((self.suffix,)))

class DefinePattern(Pattern):
    ''' Pattern : ... ;  to define a new word. '''
    def __init__(self) : super().__init__(':', ';', 'defines a new word')
    def parse(self, parser: Parser) -> Iterable[Atom]:
        word = parser.next()
        if word is None: raise ParsingIncomplete('missing word in definition')
        if not isinstance(word, Keyword): raise ParsingError(f'invalid word type {word}')
        parser.check_valid_word(word.value)
        yield Sequence(parser.parse_many((self.suffix,)))
        yield Sequence([Word(word.value)])
        yield Word('def')

class VariablePattern(Pattern):
    ''' Pattern -> ...  to define new variables. '''
    def __init__(self) : super().__init__('->', None, 'introduces variables')
    def parse(self, parser: Parser) -> Iterable[Atom]:
        content = []
        while True:
            var = parser.next()
            if var is None: raise ParsingIncomplete('missing { after ->')
            if not isinstance(var, Keyword): raise ParsingError(f'invalid variable type {var}')
            if var.value == '{': break
            parser.check_valid_word(var.value)
            content.extend([Sequence([Word(var.value)]), Word('var')])
        if len(content) == 0: raise ParsingError('missing variables after ->')
        rest = parser.parse_many(('}',))
        content.extend(rest)
        yield Sequence(content)
        yield Word('eval')
    def __str__(self) -> str:
        return f'{self.prefix}{fg.LIGHTBLACK_EX} .. {fg.RESET}' + ' { ' + f'{fg.LIGHTBLACK_EX} .. {fg.RESET}' + ' }'

class IfPattern(Pattern):
    ''' Pattern if ... else ... then, used to define conditional logic. '''
    def __init__(self) :
        super().__init__('if', 'then', 'conditional logic')
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
                raise ParsingError('A single else keyword is allowed in an if statement')
            yield from (Sequence(then_cont), Sequence(else_cont), Word('rot'), Word('?ifelse'))
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
        content = [*parser.parse_many((self.suffix, 'until', 'repeat'), ('while',))]
        if parser.closure == 'until': 
            content += [ Word('?leave') ]
        else:
            for i, atom in enumerate(content):
                if isinstance(atom, Keyword) and atom.value == 'while':
                    content = [ *content[:i], Word('not'), Word('?leave'), *content[i+1:] ]
                    break
        if any(isinstance(atom, Keyword) for atom in content):
            raise ParsingError('A single until or while keyword is allowed in a begin statement')
        yield Sequence(content)
        yield Word('forever')
    def reserved(self) -> Iterable[str]:
        yield from super().reserved()
        yield 'until' ; yield 'while' ; yield 'repeat'
    def __str__(self) -> str:
        return super().__str__() + f' | until | while {fg.LIGHTBLACK_EX}..{fg.RESET} repeat'

