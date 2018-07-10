
def repr_single(s):
    return "'" + repr('"' + s)[2:]

def repr_double(s):
    single = repr_single(s)
    return '"' + single[1:-1].replace('"', '\\"').replace('\\\'', '\'') + '"'

def test_single():
    assert r"'foobar'" == repr_single('foobar')
    assert r"'\'foobar'" == repr_single('\'foobar')
    assert "'\\'foobar'" == repr_single("'foobar")

def test_double():
    assert r'"foobar"' == repr_double("foobar")
    assert '"\'foobar"' == repr_double("'foobar")
    assert '''"\\"foobar"''' == repr_double('"foobar')
    assert '''"\\"foobar\\" = \\"blah blah\\""''' == repr_double('"foobar" = "blah blah"')

    # echo "\"foobar" > file.txt
