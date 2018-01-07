# Emeschc
[![Build Status](https://travis-ci.org/DKXXXL/emesch.svg?branch=master)](https://travis-ci.org/DKXXXL/emesch)

(A toy) (subset of) Scheme transplier to C written in Haskell.

Thanks to *Essentials of Programming Languages*

### Compile
```bash
> cd src
> ghc Emeschc.hs
```

Then the excutable file is what you want.

### Usage

Use **stdin** and **stdout**.

Make the output texts into a .c file and compile with the files in `\lib`.

### Example

*This is in Windows, Linux omitted*
```bash
> echo (letrec (((f x) (if (zerop x) 1 (* x (f (- x 1)))))) (f 10)) > ..\lib\tst
> Emeschc.exe < ..\lib\tst > ..\lib\tst.c
> cd ..\lib\
> gcc tst.c emeschlib.c gc.c -o a.exe
> a.exe
3628800.000000 

```
