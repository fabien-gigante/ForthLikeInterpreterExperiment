''' Execution engine '''

from typing import Dict, List, Iterable, Optional, Tuple, TypeVar, Union, Type, cast
from atoms import Atom, Word, Intrinsic, ExecutionError

class Scope:
    ''' Context for local variables '''
    def __init__(self, parent: Optional['Scope'] = None) -> None:
        self.parent: Optional['Scope'] = parent
        self.variables: Dict[str, Atom] = {}
    def define(self, name: str, value: Atom) -> None:
        if name in self.variables: raise ExecutionError(f'variable {Word(name)} already defined in this scope')
        self.variables[name] = value
    def store(self, name: str, value: Atom) -> None:
        scope = self.find(name)
        if scope is None: raise ExecutionError(f'variable {Word(name)} not defined')
        scope.variables[name] = value
    def find(self, name:str) -> Optional['Scope']:
        if name in self.variables: return self
        if self.parent is None: return None
        return self.parent.find(name)
    def resolve(self, name: str) -> Optional[Atom]:
        scope = self.find(name)
        return scope.variables[name] if scope is not None else None
    def list(self) -> Iterable[Tuple[str, Atom]]:
        yield from self.variables.items()
        if self.parent is not None: yield from self.parent.list()
    def open(self) -> 'Scope': return Scope(self)
    def close(self) -> 'Scope':
        if self.parent is None: raise ExecutionError('cannot close global scope')
        return self.parent

TAtom1 = TypeVar('TAtom1', bound = Atom)
TAtom2 = TypeVar('TAtom2', bound = Atom)
AtomTypeSpec = Union[Type[Atom], Tuple[Type[Atom],...]]

class Runtime:
    '''
    Runtime environement for execution.
    Holds and manages the list of available words and the stack.
    '''

    def __init__(self) -> None:
        self.words: Dict[str, Atom] = {}
        self.stack: List[Atom] = []
        self.scope: Scope = Scope()

    def check_type(self, atom: Atom, atom_type: AtomTypeSpec) -> None:
        if isinstance(atom, atom_type): return
        if isinstance(atom_type, type):
            raise ExecutionError(f'argument {atom} is not a {atom_type.__name__}')
        raise ExecutionError(f'argument {atom} is not one of {" , ".join(t.__name__ for t in atom_type)}')

    def pop_args(self, types: List[AtomTypeSpec], matching: bool = False) -> Iterable[Atom]:
        n = len(types)
        if len(self.stack) < n:
           if n > 1: raise ExecutionError(f'{n} arguments needed')
           raise ExecutionError(f'one argument needed (empty stack)')
        for i, t in enumerate(types): self.check_type(self.peek(i), t)
        if matching and len({ type(arg) for arg in self.stack[-n:] }) != 1:
            raise ExecutionError('arguments types no not match')
        for _ in range(n): yield self.stack.pop()

    def pop(self, type1: Type[TAtom1]) -> TAtom1:
        return cast(TAtom1, next(self.pop_args([type1])))

    def pop2(self, type1: Type[TAtom1], type2: Type[TAtom2], matching: bool = False) -> Tuple[TAtom1, TAtom2]:
        return cast(Tuple[TAtom1,TAtom2], tuple(self.pop_args([type1, type2], matching)))

    def peek(self, i: int = 0) -> Atom : return self.stack[-(i+1)]

    def push(self, atom: Atom) -> None:
        self.stack.append(atom)

    def is_intrinsic(self, name: str) -> bool:
        if not name in self.words: return False
        word = self.words[name]
        return any(isinstance(atom, Intrinsic) for atom in word.unbox())

    def register(self, word: str, definition: Atom) -> None:
        self.words[word] = definition

    def resolve(self, name: str) -> Atom:
        word = self.scope.resolve(name)
        if word is None and name in self.words: word = self.words[name]
        if word is None: raise ExecutionError(f'unknown word {Word(name)}')
        return word

    def execute(self, name: str) -> None:
        word = self.resolve(name)
        for atom in word.unbox(): atom.execute(self)

    def describe(self, name: str) -> str:
        if not name in self.words: raise ExecutionError(f'unknown word {Word(name)}')
        return ' '.join(f'{word}' for word in self.words[name].unbox())

class LoopInterrupt(Exception):
    ''' Raise to interrupt a loop. '''
