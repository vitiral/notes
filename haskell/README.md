# Tutorial

- http://learnyouahaskell.com
- `sudo apt-get install ghc`
- I'm using the haskell book

Stack tutorial is definitely necessary to get me up and running:
https://github.com/Originate/guide/blob/master/haskell/stack-tutorial.md

## Chapter 1: all you need is lambda
- A calculus is a "method of calculation or reasoning" -- not necessarily
  related to Calculus which is the mathematics of infinitesmal differences.
- Pure functions have _referential transparency_. Given the same inputs, they
  always return the same output.
- A "pure" langauge means that all expressions can be translated into lambda
  expressions.
- function: a relationship between a set of possible inputs to a set of
  possible outputs.
  - If the input is the same, the output _must_ be the same as well, however
    there can be the same output for multiple inputs
  - This was best described in the podcast as the function is conceptually
    a (potentially) enourmous lookup table which maps all possible inputs to
    specific outputs.

- three basic components of lambda calculus
  - expressions: superset of the other two, can be either.
  - variables: a simple variable name
  - abstractions: a function which takes an argument and has a head and body
    - For instance `𝜆x.x`
    - Since I don't want to type `𝜆` over and over again, I will use the haskell
      syntax `(\x -> x)`
  - `(\x -> x)` is _exactly the same_ function as `(\a -> a)`
  - beta reduction: the process of applying a lambda term to an argument, replacing
    the bound variables with the value of the argument, and eliminating the head.
  - applying functions is done like so
    - `(\x -> x) 2`
    - `2`
  - they are left associative
    - `(\x -> x) (\y -> y + 1) 2`
    - `[x := (\y -> y+ 1)]`
    - `(\y -> y + 1) 2`
    - `3`
  - there can also be _free variables_ which are not bound.
    - (\x -> x y)
    - NOT equivalent to `(\x -> x z)` since `z` is a different free variable.
  - `(\x y -> x y)` is shorthand for `(\x -> (\y -> x y))`

normal form (aka beta normal form) is when it cannot be reduced any further
(nothing else can be applied)

**Combinators** are a lambda term with **no free variables**. They serve only
to combine the arguments they are given.

**Divergence** is when the reduction process never terminates. Reducing should
ordinarily converge to a beta normal form, of which divergence is the opposite.
- Example: `(\x -> x x) (\x -> x x)`
- Terms that diverge simply don't produce an answer/result and are not useful.

Haskell is a typed lambda calculus with a log of surface-level decoration to
make it easier for humans to write/read. The meaning of Haskell programs
is centered around **evaluating expressions** rather than **executing
instructions**.
 - although, it can execute instructions too.


Definitions:
- abstraction: annonymous function or lambda term, i.e. `(\x -> x + 1)`
- application: how one evaluates or reduces lambdas, by binding a parameter to
  the concrete argument.
- lambda calculus: a formal system for expressing programs in terms of
  abstraction and application.
- normal order is a common _evaluation strategy_ in lambda calculi. Involves
  evaluating leftmost, outermost lambda first then nested. NOT how Haskell code
  is evaulated :D. Haskell evaluates in _call by need_.

## Chapter 2
- **redex** a reducible expressions, i.e. one not in normal form.
- **functions** map an input or set of inputs to an output


