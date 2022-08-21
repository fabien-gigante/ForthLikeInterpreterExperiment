''' All intrinsic implementations '''

from colorama import Fore as fg
from atoms import ExecutionError, Atom, Sequence, Word, Intrinsic, Literal, NumberLiteral, StringLiteral
from patterns import Pattern
from execution import Runtime, LoopInterrupt

class Print(Intrinsic):
    def __init__(self): super().__init__('.','a --  , print a')
    def execute(self, runtime: Runtime) -> None:
        print(f'  = {runtime.pop(Atom)}')

class Help(Intrinsic):
    def __init__(self): super().__init__('.w', 'print patterns and words')
    def execute(self, runtime: Runtime) -> None:
        print(fg.LIGHTBLACK_EX+'PATTERNS'+fg.RESET)
        for pattern in [cls() for cls in Pattern.classes]:
            print(f'  {pattern} {pattern.describe()}')
        print(fg.LIGHTBLACK_EX+'WORDS'+fg.RESET)
        for key in sorted(runtime.words.keys()):
            print(f'  : {fg.YELLOW}{key}{fg.RESET} {runtime.describe(key)} ;')

class PrintStack(Intrinsic):
    def __init__(self): super().__init__('.s', 'print stack')
    def execute(self, runtime: Runtime) -> None:
        print(fg.LIGHTBLACK_EX+'STACK'+fg.RESET)
        i = len(runtime.stack)
        if i > 10: 
            print(f'{fg.LIGHTBLACK_EX}  {i} :{fg.RESET}\r\t\t{runtime.stack[-1]}')
            print(f'{fg.LIGHTBLACK_EX}  ...\r\t\t...{fg.RESET}')
            i = 10 
        for atom in runtime.stack[-10:]:
            print(f'{fg.LIGHTBLACK_EX}  {i} :{fg.RESET}\r\t\t{atom}')
            i -= 1

class Define(Intrinsic):
    def __init__(self): super().__init__('def', 'a `b` --  , define word b with content a')
    def execute(self, runtime: Runtime) -> None:
        quote, definition = runtime.pop2(Sequence, Atom)
        word = quote.content[0] if len(quote.content) == 1 else None
        if not isinstance(word, Word): 
            raise ExecutionError(f'invalid word {quote} in definition')
        if runtime.is_intrinsic(word.value):
            raise ExecutionError(f'cannot redefine intrinsic {word}')
        runtime.register(word.value, definition)

class DefineVariable(Intrinsic):
    def __init__(self): super().__init__('var', 'a `b` } --  , create variable b with value a')
    def execute(self, runtime: Runtime) -> None:
        quote, value = runtime.pop2(Sequence, Atom)
        variable = quote.content[0] if len(quote.content) == 1 else None
        if not isinstance(variable, Word): 
            raise ExecutionError(f'invalid variable argument {quote}')
        if runtime.is_intrinsic(variable.value):
            raise ExecutionError(f'cannot use intrinsic {variable} as variable')
        runtime.scope.define(variable.value, value)

class Store(Intrinsic):
    def __init__(self): super().__init__('sto', 'a `b` --  , set variable b to value a')
    def execute(self, runtime: Runtime) -> None:
        quote, value = runtime.pop2(Sequence, Atom)
        variable = quote.content[0] if len(quote.content) == 1 else None
        if not isinstance(variable, Word): 
            raise ExecutionError(f'invalid variable argument {quote}')
        if runtime.is_intrinsic(variable.value):
            raise ExecutionError(f'cannot use intrinsic {variable} as variable')
        runtime.scope.store(variable.value, value)

class PrintVariables(Intrinsic):
    def __init__(self): super().__init__('.v', 'print variables')
    def execute(self, runtime: Runtime) -> None:
        print(fg.LIGHTBLACK_EX+'VARIABLES'+fg.RESET)
        for name, value in runtime.scope.list():
            print(f'  {Word(name)} = {value}')

class Add(Intrinsic):
    def __init__(self): super().__init__('+', 'a b -- a+b')
    def execute(self, runtime: Runtime) -> None:
        arg2, arg1 = runtime.pop_args([(Literal, Sequence), (Literal, Sequence)], True)
        if isinstance(arg1, Sequence) and isinstance(arg2, Sequence):
            runtime.push(Sequence([*arg1.content, *arg2.content]))
        elif isinstance(arg1, NumberLiteral) and isinstance(arg2, NumberLiteral):
            runtime.push(NumberLiteral(arg1.value + arg2.value))
        elif isinstance(arg1, StringLiteral) and isinstance(arg2, StringLiteral):
            runtime.push(StringLiteral(arg1.value + arg2.value))
        else: raise ExecutionError(f'Invalid argument types for {self.value}')

class Substract(Intrinsic):
    def __init__(self): super().__init__('-', 'a b -- a-b')
    def execute(self, runtime: Runtime) -> None:
        arg2, arg1 = runtime.pop2(NumberLiteral, NumberLiteral)
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
        else: raise ExecutionError(f'Invalid argument types for {self.value}')

class Divide(Intrinsic):
    def __init__(self): super().__init__('/', 'a b -- a/b')
    def execute(self, runtime: Runtime) -> None:
        arg2, arg1 = runtime.pop2(NumberLiteral, NumberLiteral)
        runtime.push(NumberLiteral(arg1.value / arg2.value))

class Equals(Intrinsic):
    def __init__(self): super().__init__('=', 'a b -- a=b')
    def execute(self, runtime: Runtime) -> None:
        arg2, arg1 = runtime.pop2(Literal, Literal)
        runtime.push(NumberLiteral(1 if arg1.value == arg2.value else 0))

class LowerThan(Intrinsic):
    def __init__(self): super().__init__('<', 'a b -- a<b')
    def execute(self, runtime: Runtime) -> None:
        arg2, arg1 = runtime.pop2(Literal, Literal, True)
        if isinstance(arg1, NumberLiteral) and isinstance(arg2, NumberLiteral):
            runtime.push(NumberLiteral(1 if arg1.value < arg2.value else 0))
        elif isinstance(arg1, StringLiteral) and isinstance(arg2, StringLiteral):
            runtime.push(NumberLiteral(1 if arg1.value < arg2.value else 0))
        else: raise ExecutionError(f'Invalid argument types for {self.value}')

class GreaterThan(Intrinsic):
    def __init__(self): super().__init__('>', 'a b -- a>b')
    def execute(self, runtime: Runtime) -> None:
        arg2, arg1 = runtime.pop2(Literal, Literal, True)
        if isinstance(arg1, NumberLiteral) and isinstance(arg2, NumberLiteral):
            runtime.push(NumberLiteral(1 if arg1.value > arg2.value else 0))
        elif isinstance(arg1, StringLiteral) and isinstance(arg2, StringLiteral):
            runtime.push(NumberLiteral(1 if arg1.value > arg2.value else 0))
        else: raise ExecutionError(f'Invalid argument types for {self.value}')

class Clear(Intrinsic):
    def __init__(self): super().__init__('clear', 'a1 .. an --')
    def execute(self, runtime: Runtime) -> None:
        runtime.stack = []

class Depth(Intrinsic):
    def __init__(self): super().__init__('depth', 'a1 .. an -- a1 .. an n')
    def execute(self, runtime: Runtime) -> None:
        runtime.push(NumberLiteral(len(runtime.stack)))

class Evaluate(Intrinsic):
    def __init__(self): super().__init__('eval', 'a -- eval of a' )
    def execute(self, runtime: Runtime) -> None:
        arg = runtime.pop(Atom)
        if isinstance(arg,Sequence): runtime.scope = runtime.scope.open()
        try:
            for atom in arg.unbox(): atom.execute(runtime)
        finally:
            if isinstance(arg,Sequence): runtime.scope = runtime.scope.close()

class Leave(Intrinsic):
    def __init__(self): super().__init__('leave', 'interrupts loop')
    def execute(self, runtime: Runtime) -> None: raise LoopInterrupt()

class Forever(Intrinsic):
    def __init__(self): super().__init__('forever', 'a -- , evaluates a forever' )
    def execute(self, runtime: Runtime) -> None:
        arg = runtime.pop(Sequence)
        try:
            while True:
                runtime.push(arg)
                runtime.execute('eval')
        except LoopInterrupt: pass

class EvaluateIf(Intrinsic):
    def __init__(self): super().__init__('?if', 'a b -- eval of a if b')
    def execute(self, runtime: Runtime) -> None:
        cond = runtime.pop(NumberLiteral)
        if cond.value == 0: runtime.pop(Atom)
        else: runtime.execute('eval')

class Prepend(Intrinsic):
    def __init__(self): super().__init__('<+', 'a1 {a2 .. an} -- {a1 a2 .. an}')
    def execute(self, runtime: Runtime) -> None:
        seq, atom = runtime.pop2(Sequence, Atom)
        runtime.push( Sequence([atom, *seq.content]) )

class Postpend(Intrinsic):
    def __init__(self): super().__init__('+>', 'a1 {a2 .. an} -- {a2 .. an a1}')
    def execute(self, runtime: Runtime) -> None:
        seq, atom = runtime.pop2(Sequence, Atom)
        runtime.push( Sequence([*seq.content, atom]) )

class Reverse(Intrinsic):
    def __init__(self): super().__init__('reverse', '{a1 .. an} -- {an .. a1}')
    def execute(self, runtime: Runtime) -> None:
        seq = runtime.pop(Sequence)
        runtime.push( Sequence(reversed(seq.content)) )

class Accept(Intrinsic):
    def __init__(self): super().__init__('accept', ' -- a ; user entered string')
    def execute(self, runtime: Runtime) -> None:
        runtime.push( StringLiteral(input('  ? ')) )

class Page(Intrinsic):
    def __init__(self): super().__init__('page', 'clear screen')
    def execute(self, runtime: Runtime) -> None: print('\033[2J', end='')
