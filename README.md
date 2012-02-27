# Mu

**A tiny language to practice your u-recursion ;)**

## Usage

The interpreter is written in Haskell. It was tested with ghc6. To run the interpreter on a .mu file, for example, example.mu, you need to either:

    make
    ./mu example.mu

or

    runhaskell Mu.hs example.mu

Of course, you should have ghc installed, for Ubuntu, you do:

    sudo apt-get install ghc

## Language Syntax

The language has very minimal syntax:

1. Comment lines start with %
2. One function declaration per line.
3. Functions must use functions only defined before them in the file.
4. The constructs are `zero`, `succ`, `chi[f; g1, g2, ..]` (composition), `pi[n; k]` (projection), `rho[g, h]` (primitive recursion) and `mu[f]` (unbounded minimization).
5. No nested declarations, each declaration must be one of the constructs described above with the argument functions defined before (or one of zero and succ).
6. See example.mu for more details.

## License

*Permission is hereby granted to you to do whatever you want with the code, just don't bother the author (except with bug reports or improvement suggestions of course ;)).*
