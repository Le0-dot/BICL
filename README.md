# BICL - Because I Can Lang


An experimental language to test out new ideas and implement some of the more interesting algorithms used in compilers and iterpreters.

## Example systax
```
export foo = fn arg -> arg // A fuction that will be exported from the module

define bar = fn arg1 arg2 -> arg1 // Immutable function local to module

define pi = 3.1415 // Immutable constant local to module

export longfunc = fn args -> do
    let abc = 123 // Define a variable
    abc = 321 // Mutate a variable
    let fun = fn a b -> do
        a = 123 // Mutate parameter
        b = 321
        a // No return keyword, last expression in `do` block is returned
    let abd = fun abc 123 // abd == 123, abc == 321, since the parameter was copied and not passed by reference
```
