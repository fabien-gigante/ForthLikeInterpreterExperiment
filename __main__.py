''' FOLIE : FOrth-Like Interpreter Experiment '''

import os
import colorama
from colorama import Fore as fg

from atoms import Intrinsic, Error
from parsing import Parser, Pattern, ParsingIncomplete
from execution import Runtime, ExecutionError, LoopInterrupt
import patterns    # ignore 'Unused import' warning, actually usefull
import intrinsics  # ignore 'Unused import' warning, actually usefull

class Interpreter:
    ''' The interpreter program '''

    def __init__(self) -> None:
        os.system('')
        colorama.init(convert = True, strip = False)
        self.prompt = fg.LIGHTWHITE_EX + '>> ' + fg.RESET
        self.prompt_continued = fg.LIGHTWHITE_EX + '.. ' + fg.RESET
        self.showstack = True
        self.runtime = Runtime()
        for intrinsic in Intrinsic.classes: intrinsic().register(self.runtime)
        self.parser = Parser()
        for pattern in Pattern.classes: pattern().register(self.parser)
        for instruction in Interpreter.BOOTSTRAP: self.execute(instruction)
        print(f'Welcome to {fg.LIGHTWHITE_EX}FOLIE{fg.RESET} {fg.GREEN}( FOrth-Like Interpreter Experiment ){fg.RESET}.')
        print(f'Type {fg.YELLOW}help{fg.RESET} for available patterns and verbs.\n')
        print(f'Examples:{fg.LIGHTBLACK_EX} ( alternative implementations of the fatcorial function )')
        print('  :! (a -- a!)  dup 0 > if dup 1 - ! * else drop 1 then;                6 ! .')
        print('  :! (a -- a!) 1 { over 0 <= ?leave over 1 - -rot * } forever nip;      6 ! .')
        print('  :! (a -- a!) -> n { `n n 1 - ! *` 1  n 0 > ?ifelse };                 6 ! .')
        print('  :! (a -- a!) -> n { 1 begin n 0 > while n * n 1 - `n` sto repeat };   6 ! .')
        print('  :! (a -- a!) 1 1 rot for i i * next;                                  6 ! .' + fg.RESET);

    def execute(self, expression: str) -> None:
        self.parser.execute(self.runtime, expression)

    BOOTSTRAP = [
        ':print (alias) .; :stack (alias) .s; :help (alias) .w; :vars (alias) .v; ',
        ':prepend (alias) <+; :append (alias) +>; :bye (alias) leave;',
        ':drop (a --) {} +> 0 * eval;',
        ':dup (a -- a a) {} +> 2 * eval;',
        ':swap (a b -- b a) {} +> +> eval;',
        ':rot (a b c -- b c a) {} +> <+ +> eval;',
        ':-rot (a b c -- c a b) rot rot;',
        ':nip (a b -- b) swap drop;',
        ':tuck (a b -- b a b) dup -rot;',
        ':over (a b -- a b a) swap tuck;',
        ':neg (a -- -a) 0 swap -;',
        ':not (a -- boolean not of a) 0 1 rot ?ifelse;',
        ':<= (a b -- a<=b) > not; :>= (a b -- a>=b) < not; :<> (a b -- a<>b) = not;',
        ':?ifelse (a b c -- a !, if c | b !, otherwise) `swap` swap ?if swap drop eval;',
        ':?leave ( a -- interrupts if a) if leave then;'
    ]

    def execute_input(self) -> None:
        input_str = '' ; prompt = self.prompt
        while True:
            input_str += input(prompt)
            try:
                self.execute(input_str)
            except ParsingIncomplete:
                input_str += '\n'
                prompt = self.prompt_continued
            else: break

    def loop(self) -> None:
        while True :
            if self.showstack: print(); self.execute('.s')
            try:
                self.execute_input()
            except LoopInterrupt: 
                break
            except Error as error:
                print(error)
            except KeyboardInterrupt:
                print(ExecutionError('execution interupted by user'))
        print('\nSee you soon !\n')

# Main function calling
if __name__ == '__main__':
    Interpreter().loop()
