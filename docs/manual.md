# Klaus Programming Language Manual

## Introduction
Klaus is a stack-based programming language that operates using a last-in, first-out (LIFO) data structure. It provides a simple instruction set for arithmetic operations, stack manipulation, conditional execution, loops, and variable storage. The compiler is written for x86-64 Linux. You can find out more about the language (and my other projects) on my blog: https://blog.julianlimburg.zip

## Building and using the compiler
Klaus uses my [noob build system](https://github.com/nailuj05/blog), just build noob using a C compiler and run the executable to build the klaus compiler.
The compiler itself takes in an input file (usually with a `.kl` extension) and a optional output filename, optional debug information can be included with `-d`.

## Syntax and Tokens
Klaus operates by processing a series of tokens, each representing a specific instruction. Tokens are *whitespace-separated* words. Comments begin with `#` and extend to the end of the line.

## Instructions

### Stack Operations
- `<int>`: Pushes an integer onto the stack.
- `.`: Removes the top value from the stack.
- `dup`: Duplicates the top value on the stack.
- `swap`: Swaps the top two values on the stack.

### Input/Output
- `puts`: Prints the top value on the stack.
- `read`: Reads an integer from input and pushes it onto the stack.

### Arithmetic Operations
These operations pop two values from the stack, perform the operation, and push the result.
- `+`: Adds two values.
- `-`: Subtracts the second value from the first.
- `*`: Multiplies two values.
- `/`: Divides the first value by the second.
- `%`: Computes the remainder of division.

### Comparison Operators
Each comparison pops two values and pushes `1` if true, `0` otherwise.
- `<`  Less
- `<=` Less or equal
- `>`  Bigger
- `>=` Bigger or equal
- `==` Equal
- `!=` Not equal

### Conditional Execution
- `if`: Pops the top value; if it is not zero the following code will be executed, other wise jumps to matching `end`. 
- `end`: Marks the end of an `if` block.

### Loops
- `loop`: Marks the beginning of a loop.
- `end`: Jumps back to the corresponding `loop`.
- `break`: Exits the loop by jumping to corresponding `end`.

### Variable Storage
- `:<name>` : Variables are declared and set using a `:` followed by their name, the current top of the stack will be stored in the variable. 
- `<name>`: Pushes the value of the variable onto the stack.

### Scopes and Spaces
> both scopes and spaces are mostly syntactic sugar, but they allow you to write more concise (and readable code)

Scopes in klaus work similar to how you would expect them from C, meaning the stack gets reset at the end of the scope. 
This is handy for doing calculations that you just want to store in a variable or print without putting a lot of unused values on the stack or needing to pop them all off manually.
- `{`: Begins a new scope.
- `}`: Ends the current scope.

Spaces work similarly to scopes, however here the this time the top of the stack is kept. 
This is handy for if statements where you might need some calculations and comparisons but only care about the 0/1 result.
- `BeginSpace` (`(`): Marks a new stack frame.
- `EndSpace` (`)`): Ends the stack frame.

**Note** scopes and spaces do not have a seperate stack frame, if you pop more values from the stack than you pushed onto it you will access the values outside the scope/space.
Futhermore, at the end of the scope the top of the stack will be restored. Any values you might have popped of will be restored. 
In general it is adviced to avoid doing this as it might lead to unpredictable behaviour.

### Program Control
- `exit`: Terminates execution immediately.

## Example Programs
You can find more programs in the examples folder.

### Fibonacci Calculation
```klaus
read :n
0 1

# edge case n = 0
{ n 0 == if 0 puts exit end }

# loop
loop
    # decrement n and exit if 0
    ( n 1 - :n 0 n >= ) if break end
    :prev +
    prev swap
end

# result
puts
```

## Notes
- Variables must start with a letter or `_` and can contain numbers.

This concludes the basic manual for Klaus. Happy coding!

