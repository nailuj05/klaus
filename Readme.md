# Klaus

Klaus is a simple stack based language with a compiler implemented in OCaml. 
The language itself does not rely on any dependencies, it is currently developed on and for x86-64 GNU/Linux, 
but exploring other architectures and operating systems is something I would like to do.

Its name is a reference to [Staplerfahrer Klaus](https://www.youtube.com/watch?v=dJdCJMyBi5I) (Stapel being Stack in german).

## Building the compiler

This project is build using my [noob build system](https://github.com/nailuj05/noob) on Linux. 
You will have to build the noob.c file using your systems C compiler, then run the executable to build the main compiler using ocamlc.

---

## The Klaus language

Being a stack based language means there are no variables, all data is kept on a stack.
Klaus provides you with a basic set of input/output, algebraic and branching/looping instructions.
All data handled by klaus is signed 64-bit integers.
~~Currently the syntax is still in a very simple, pseudo assembly state,
I am planning on expanding the synatx and allowing for more complex, abstract syntax.~~

I am currently reworking the language for a brand new syntax and control flow. More info to follow soon. 

Many more examples can be found in the `examples/` folder (most of the examples there still use the old synatx, new ones will follow)

## Operators

- `<number>`: Pushes an immediate value onto the stack
- `.`: Pops the top value of the stack
- `puts`: Prints the top value of the stack (without popping it)
- `read`: Reads a user input (as int) and pushes it on top of the stack
- `swap`: Swaps the top 2 values
- `dup`: Duplicates the top value
- `end` : Ends the program
- `+`: Replaces the top 2 values by their sum
- `-`: Replaces the top 2 values by their difference
- `*`: Replaces the top 2 values by their product
- `/`: Replaces the top 2 values by their quotient
- `:<name>`: Defines a variable with given name and stores the top of the stack within
- `<name>`: Pushes the variable given by the name onto the stack

## Future
- [x] Replace libc
- [ ] Explore Reverse Polish Notation -> currently in progress
- [ ] Expand documentation and example programs
- [ ] Char, String and float datatypes

*Further information about the language will follow*

