# PG-64: Chapter 10

## Bayes rule and probablility
- If A and B are independnet then `P(A and B) == P(A)*P(B)`
- else (in general) `P(A and B) == P(A given B) * P(B)`
- If A and B are mutually exclusive (if one happens the other can't)
  then `P(A or B) == P(A) + P(B)`. Obviously `P(A and B) == 0`
- Else (in general), `P(A or B) == P(A) + P(B) - P(A and B)`

## n choose k problem:
- `C(n, k) = (n-1 choose k) + (n-1 choose k-1)`
- `C(n, k) = n! / (k! * (n-k)!)`

If I have 10 numbers and want to select 3, where order doesn't matter

10 * 9 * 8 = 10! / (n-k)!
but actual = 10! / (k! * (n-k)!)
           = 10 * 9 * 8 / 6

... the 6 is simply to remove the fact that we don't care about order!

`n! / (n-k)!` gives you the number of results _if the order matters_.
If the order doesn't matter you need to divide by `k!` to remove the
additional bump that caring about ordering gives!

## Binomial distribution:
- given a set of events each with probability p, the probability
  of exactly k events of n is:
  `P(n, k) = C(n, k) * (1 - p)^(n - k) * p^k
- Breaking it down:
  (combinations of independent events)
  times (probability of event k times in a row)
  times (probability of non-event n-k times in a row)
- For hard drives: the probability that _exactly_ 8 hard drives fail
  is: (probability 8/8 hard drives fail)
     times (probabilty 92/92 hard drives don't fail)
     times (number of combinations 100 choose 8)
  - If you want to know probability 8 or _more_ fail you need to sum them.


# Example
> Given two numbers m and n, write a method to return the first
> number r that is divisible by both (eg, the least common multiple)

This is the least common divisor.

We know that all numbers are divisible by a series of prime numbers, and that
a prime will always be the least common divisor.

So if we split each number up into a set of it's primes we just have to
find where the sets overlap and take the (non-1) min.

We can use the sieve of erathsmus to find the primes:
- start at p=2 and an array of length sqrt(n) + 1
- mark all indexes that are a multiple of p
- set p = the next index that is unmarked and repeat
- if no such indexes remain, quit.

# 10.1
> You have a basketball hoop and someone says that you can play 1 of 2 games
>
> - Game #1: You get one shot to make the hoop
> - Game #2: You get three shots and you have to make 2 of 3 shots
>
> If p is the probability of making a particular shot, for which values of p
> should you pick one game or the other?

Game1 chance of winning = p

Game2 chance of winning =
- chance of making all 3 =
```
p * p * p

= p^3
```

Note that there is only _one permutation_ where you make all 3.

- chance of making 2/3 =
```
(1-p) * p * p +  # first permutation
p * (1-p) * p +  # second permutation
p * p * (1-p)    # third permutation

= 3 * (1-p)^1 * p^2
```

Note the 3 possible permutations leads to it being multiplied by 3.

This can be simplified to `C(n, k) * (1 - p)^(n-k) * p^k`


So:
```
p^3 + 3(1-p)p^2
= p^3 + 3p^2 - 3p^3
= -2p^3 + 3p^2
```

The point where these are equal is:

```
p = -2p^3 + 3p^2
-2p^3 + 3p^2 -p = 0
= -2p^2 + 3p - 1
= 2p^2 - 3p + 1
= ( 2p - 1 ) * ( p - 1 )

* p = 1/2
* p = 1


1/4 <> -2 * 1/64 + 3 * 1/16
       -1/32 + 3/16
```

- p = 1/2 ==> doesn't matter
- p = 1   ==> doesn't matter lol
- p < 1/2 ==> choose game 1
- p > 1/2 ==> choose game 2

## Wrong
okay...

let `s(k, n)` be probability of making k shots out of n.

`s(k, n) = C(n, k) (1 - p)^(n - k) * p^k


# 10.2
> There are three ants on different vertices of a triangle
>
> What is the probability of collision (between any two or all of them) if they
> start walking on the sides of the triangle?
>
> Similarly find the probability of collision with ‘n’ ants on an ‘n’ vertex polygon

First of all, I assume the ants:
- Walk at the same speed
- Can walk in a random direction
- Walk continuously around the polygon.

If so, the chance of collision is simply the chance that 1 or more ants is walking a
different direction than the others.

The chance that all ants are walking left out of N ants is. `1 / N` It is the
same for right. Therefore the probability they are all the same direction
is ( 2 / 2^N) so the probability of collision is: (1 - 2 / 2^N)

For a triange this is 1 - 2/8 = 3/4

# 10 3
> Given two lines on a Cartesian plane, determine whether the two lines would
> intersect

If the lines are parallel, they will not intersect. Else they will.

Whether the lines are parallel or not can be determined by their slope. Their
slope can be found by rise/run = `m = (y2 - y1) / (x2 - x1)`

If their slopes are equal and they are not the same line then they are parallel
and will not intersect.


```
smallest_unit = 1e-6

class Line(object):
    def __init__(self, slope, yintercept):
        self.slope = slope
        self.yintercept = yintercept

    def intersect(self, other):
         # Note: if no yintercept then yintercept == infinity
        return (
            (math.abs(self.slope - other.slope) < smallest_unit)
            or (math.abs(self.yintercept - other.yintercept) < smallest_unit)
        )

```

# 10.4
> Write a method to implement `*, - , /` operations
>
> You should use only the `+` operator

Assuming only integers

```
def negate(y):
    """Return -y"""
    if y < 0:
        d = 1
    else:
        d = -1

    result = 0
    while y != 0:
        result = result + d
        y = y + d

    return result


def subtract(x, y):
    """perform x - y
    """
    return x + negate(y)

def multiply(x, y):
    result = 0
    if y > 0:
        for _ in range(y):
            result = result + x
    else:
        y = subtract(0, y)
        for _ in range(y):
            result = subtract(result, x)

def divide(x, y):
    """Perform x / y.

    Gradeschool long division
        15 R7
      ---------
    9 |142 (9 goes into 14 1 time)
       -90 (9 times 1 is 9)
        --
        52 (9 goes into 52 5 times)
       -45
       ---
         7

    For our purposes we can just ask
    "how many times does y go into x?"
    """
    count = 0

    # ensure both are postive
    if x < 0:
        x_neg = True
        x = negate(x)
    else:
        x_neg = False

    if y < 0:
        y_neg = True
        y = negate(y)
    else:
        y_neg = False

    while x > y:
        x = subract(x, y)
        count += 1

    remainder = x

    if (x_neg and y_neg) or (not x_neg and not y_neg):
        pass # stay positive
    else:
        count = negate(count)

    return count, remainder
```

# 10.5
> Given two squares on a two dimensional plane, find a line that would cut
> these two squares in half.


