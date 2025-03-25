# Klaus

Klaus is a simple stack based language with a compiler implemented in OCaml. 
The language itself does not rely on any dependencies, it is currently developed on and for x86-64 GNU/Linux, 
but exploring other architectures and operating systems is something I would like to do.

Its name is a reference to [Staplerfahrer Klaus](https://www.youtube.com/watch?v=dJdCJMyBi5I) (Stapel being Stack in german).

I wrote about building klaus in my blog: https://blog.julianlimburg.zip

## Building the compiler

This project is build using my [noob build system](https://github.com/nailuj05/noob) on Linux. 
You will have to build the noob.c file using your systems C compiler, then run the executable to build the main compiler using ocamlopt.

---

## The Klaus language

Being a stack based language means you are mostly working on LIFO stack (but klaus also provides variables).
Klaus provides you with a basic set of input/output, algebraic and branching/looping instructions.
All data handled by klaus is signed 64-bit integers.

A complete manual of the language can be found here: https://github.com/nailuj05/klaus/blob/main/docs/manual.md

Examples can be found in the `examples/` folder (more will follow)

## Future
- [x] Replace libc
- [x] Explore Reverse Polish Notation -> currently in progress
- [x] Expand documentation and example programs
- [ ] Char, String and float datatypes

*Further information about the language will follow*

