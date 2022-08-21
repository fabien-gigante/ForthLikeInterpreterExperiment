''' Base classes from atoms and errors '''

from typing import Set, Iterable, Optional, Union, Type, TYPE_CHECKING
from colorama import Fore as fg
if TYPE_CHECKING: from execution import Runtime

class Error(Exception):
    ''' Abstract. Applicative Error. Rendered in red. '''
    def __init__(self, msg) -> None:
        super().__init__(f'{fg.LIGHTRED_EX}ERROR:{fg.RESET} {msg}')

class ExecutionError(Error):
    ''' Raised during execution. '''

class Atom:
    ''' Abstract. Smallest element of language. '''
    def unbox(self) -> Iterable['Atom']:
        raise ExecutionError(f'atom {self} cannot be unboxed')
    def execute(self, runtime: 'Runtime') -> None:
        raise ExecutionError(f'atom {self} cannot be executed')

class Literal(Atom):
    ''' Abstract. Atomic int, float or string literal value. '''
    def __init__(self) :
        self.value : Union[int, float, str]
    def unbox(self) -> Iterable[Atom]: yield self
    def execute(self, runtime: 'Runtime') -> None:
        runtime.push(self)

class NumberLiteral(Literal):
    ''' Atomic int or float literal value. '''
    def __init__(self, value: Union[int,float]) -> None:
        super().__init__()
        self.value : Union[int, float] = value
    def __str__(self) -> str:
        return f'{fg.CYAN}{self.value}{fg.RESET}'

class StringLiteral(Literal):
    ''' Atomic string literal value. '''
    def __init__(self, value: str) -> None:
        super().__init__()
        self.value : str = value
    def __str__(self) -> str:
        return f"{fg.CYAN}'{self.value}'{fg.RESET}"

class Comment(Atom):
    ''' A piece of comment string. '''
    def __init__(self, value: str) -> None:
        self.value = value
    def __str__(self) -> str:
        return f'{fg.GREEN}({self.value}){fg.RESET}'
    def execute(self, runtime: 'Runtime') -> None: pass

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
        if len(self.content) == 1 and isinstance(self.content[0], Word):
            return f'`{self.content[0]}`' # prefered notation in this case
        return '{ ' + ' '.join( f'{atom}' for atom in self.content ) + ' }'
    def execute(self, runtime: 'Runtime') -> None:
        runtime.push(self)
    def unbox(self) -> Iterable[Atom]: yield from self.content

class Intrinsic(Atom):
    ''' Intrinsic implementation of a word. '''
    classes : Set[Type['Intrinsic']] = set()
    def __init_subclass__(cls) -> None: Intrinsic.classes.add(cls)
    def __init__(self, value: str = '', comment: Optional[str] = None):
        self.value = value
        self.comment = comment
    def register(self, runtime: 'Runtime'):
        if self.comment is None :
            runtime.register(self.value, self)
        else:
            runtime.register(self.value, Sequence([Comment(self.comment), self]))
    def __str__(self) -> str:
        return f'{fg.LIGHTBLACK_EX}intrinsic<{type(self).__name__}>{fg.RESET}'
