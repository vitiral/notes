'''remove all duplicates from a given string'''

def simple_rm_dups(s):
    return ''.join(set(s))


def rm_dups_preserve_order(s):
    found = set()
    out = []
    for c in s:
        if c not in found:
            found.add(c)
            out.append(c)
    return ''.join(out)


assert set('abc') == set(simple_rm_dups('aacbbc'))
assert 'acb' == rm_dups_preserve_order('aacbbc')
