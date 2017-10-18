""" Find Excel column name from a given column number

MS Excel columns has a pattern like A, B, C, … ,Z, AA, AB, AC,…. ,AZ, BA, BB, …
ZZ, AAA, AAB ….. etc. In other words, column 1 is named as “A”, column 2 as
“B”, column 27 as “AA”.

Given a column number, find its corresponding Excel column name. Following are
more examples.

Design:
- column name is basically a base-26 number, where A=0, B=1, C=2, ...
- column number then needs 1 added to it, since it is 1 index


Let's do a similar system but using only A and B

base10  binary  ex-bin  ex-bin-1
0       0       A
1       1       B       A           n=0;                                                     digit(n % 2)   0
2       10      AA      B           n=1;                                                     digit(n % 2)   0
3       11      AB      AA          n=2;                           digit((n // 2) % 2 - 1) + digit(n % 2)   1
4       100     BA      AB          n=3;                           digit((n // 2) % 2 - 1) + digit(n % 2)   1
5       101     BB      BA          n=4;                           digit((n // 2) % 2 - 1) + digit(n % 2)   2
6       110     AAA     BB          n=5;                           digit((n // 2) % 2 - 1) + digit(n % 2)   2
7                       AAA         n=6; digit((n // 4) % 2 - 1) + digit((n // 2) % 2 - 1) + digit(n % 2)   3
8                       AAB         n=7; digit((n // 4) % 2 - 1) + digit((n // 2) % 2 - 1) + digit(n % 2)   3
                                                                                                            3
                                                                                                            3

def digit(num):
    # num is value 1 - 2 (26 in the future)
    return 'A' + num


def excel(num, base=2):
    assert num >= 1
    num -= 1

    out = [digit(num % base)]
    num = num // base
    while num > 0:
        out.append(digit(num % base - 1))
        num = num // base

    out.reverse()
    return ''.join(out)





We notice that they don't share a whole lot in common at first glance
"""

examples = [
    (1,              'A'),
    (26,             'Z'),
    (27,             'AA'),
    (51,             'AY'),
    (52,             'AZ'),
    (80,             'CB'),
    (676,            'YZ'),
    (702,            'ZZ'),
    (705,            'AAC'),
]


def col_digit(value):
    """Convert an integer to 'column digit' notation."""
    assert value <= 25
    return chr(ord('A') + value)


def col_val(sig, digit):
    """Convert a digit to a value, including it's significance place."""
    return (sig) * 26 + (ord('Z') - ord('A') - digit + 1)


def col_name(col_num):
    """Given a column number (index 1) return the column name.

    Unlike a normal numbering system, the values are just
    the sum of the values along with their significance.
    """
    if col_num < 1:
        raise ValueError("col_num has a minimum value of 1")

    # In the same way as dividing by 10 shifts digits for base 10,
    # dividing by 26 will reduce digits for base 26
    #
    # example: 499 / 10 = 49 + remainder 9
    #
    # ... except that's not true. A represents 1 on the left side
    # and 0 otherwise...
    # ... no... it is like counting using tally's.
    #
    # - A represents 1
    # - AA represents 1 * 26 + 1
    # - AAA represeents 1 * 26^2 + 1*26 + 1
    # - BAA represents 2*26^2 + 1*26 + 1
    # etc
    # there is no way to represent 0
    #
    # Okay... but how do you count this in reverse...

    digit = 0
    col = []
    col.append(col_num % 26)
    col_num = col_num // 26
    while col_num > 0:
        col.append(col_num % 26)
        col_num = col_num // 26

    col.reverse()
    print(col)

    digits = []
    for n in col:
        if n:
    return ''.join(col_digit(d + 1)
