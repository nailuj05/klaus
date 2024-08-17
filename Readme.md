# Klaus

Klaus is a simple stack based language with a compiler implemented in OCaml. 
Its name is a reference to [Staplerfahrer Klaus](https://www.youtube.com/watch?v=dJdCJMyBi5I) (Stapel being Stack in german).

## Building the compiler

This project is build using my [noob build system](https://github.com/nailuj05/noob) on Linux. 
You will have to build the noob.c file using your systems C compiler, then run the executable to build the main compiler using ocamlc.

---

## The Klaus language

Being a stack based language means there are no variables, all data is kept on a stack. 
Klaus provides you with a basic set of input/output, algebraic and branching/looping instructions.

```klaus
# Example program that compares a user input to 10 and prints the result
Read
Push 10
Cmp < :bigger
Push 0
Puts
End
:bigger
Push 1
Puts
```

All data handled by klaus is signed 64-bit integers.

```klaus
# Fibonacci numbers
```

*Further information about the language will follow*

