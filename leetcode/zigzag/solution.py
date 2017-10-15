"""Create a zigzag function:
"""
def zigzag(text, rows):
    if rows == 1:
        return text

    if rows % 2 == 0:
        raise ValueError("rows is even")

    flat = []

    # pading on the zig-cols
    pad = (rows // 2) * ' '
    empty = rows * ' '

    # each time row > rows switch even col
    # when even_col == True only put one character
    # right in the middle
    row = 0
    padded_cols = False
    for ch in text:
        if padded_cols:
            flat.extend([empty, pad, ch, pad, empty])
            padded_cols = False
        else:
            flat.append(ch)
            row += 1

        # if we have written the full col,
        # we need to switch to a padded col
        if row == rows:
            row = 0
            padded_cols = True

    # make the output a list of single chars
    flat = list(''.join(flat))

    # now reorgnize to break into rows
    out = []
    for row in range(rows):
        row_out = []
        for i in range(len(flat)):
            if not (i*3 + row < len(flat)):
                break
            row_out.append(flat[i*3 + row])
        out.append(''.join(row_out))
    return '\n'.join(out).strip()

example = "PAYPALISHIRING"
expected = '''\
P   A   H   N
A P L S I I G
Y   I   R\
'''

result = zigzag(example, 3)
assert result == expected, '\n' + result + '\n{}\n{}'.format(repr(result), repr(expected))
