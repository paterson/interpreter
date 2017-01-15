# CS4012: Lab 2

### Interpreter and Debugger

## Milestones

- [x] Convert project to Stack project
- [x] Step through program
- [x] Inspect variable values
- [x] Inspect History of variable
- [x] Step Backwards through program
- [x] Static Analysis to detect unused variables
- [x] Bonus: Interactively execute arbitrary statements at any stage.

## Screenshot

![Screenshot](http://i.imgur.com/oMROMii.png)

* The file column is the statements that have been executed, and the statement that is next to be executed.

* The variables column is the current environment.

* The output column is the accumulated output for the program so far.

## Setup

Build the stack project:
```bash
stack build
```

The program can be executed by passing in the path to the file as the first argument.

Run the interpreter with the input file test.input:
```bash
stack exec interpreter-exe tests/test.input
```

To test the static analysis for detecting unused variables, just use the failingtest.input file:
```bash
stack exec interpreter-exe tests/failingtest.input
```

## Commands
 * `next` : Execute the next statement
 * `back` : Step back and undo the last statement
 * `:<variablename>` : Input a variable's name to view it's history
 * `|<Statement>`: Execute a statement at the current point of the program. E.g `|Print (Const (I 10))`
