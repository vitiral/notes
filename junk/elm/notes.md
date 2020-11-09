
# general layout
generally the concepts look fairly rustic, with the addition of immutability.

types must match and a strictly enforced. Enum's exist and there is pattern
matching.

It also looks like elm works on expressions

# operators
All operators behave as expected except:

- `++`: seems to the the same as `concat`
- / vs // is same as python
- x = if False then A else B // x == B
- `|` is completely different, it is the "update" operator.
    { point | y = 3 } will return a new Point with y changed

# declaring types
- declare a type alias: `type alias Model = Int`
- declare an enum: `type Msg = Increment | Decrement`

# declaring functions
function declaration is pretty unusual (for me)
- ananymous functions seem similar to a lambda
    `divide = \n -> n / 2`
    You can use the anonymous function directly
    `(\n -> n / 2) 128` returns 64
    The reason `\` was chosen is because it is supposed to look like a
    lambda, which is freaking terrible, lol

- normal declaration is just shorthand for anonymous functions
    `divide x y = x / y` is just shorthand for
    `divide = `\x y -> x / y`
    actually you can break ths down more because of the syntax
    `divide = `\x -> (\y -> x / y)`

All functions take a single argument. "multiple argument functions" actually accept a
single argument *and return a function*. If you provide multiple arguments, the next
argument get's called by the returned function.

This is literal, you can say
```
divide denominator numerator =
half = divide 2
```

and that works as you would expect

The full function definition looks like:
```
divide : Float -> Float -> Float
divide   x        y =
    x / y
```

# types

## Basic Types
```
-- a linked list containing integers
type IntList = Empty | Node Int IntList
```

## Generic types
```
type List a = Empty | Node a (List a)
```
In this example, `a` is a *type variable*


