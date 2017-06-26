
import pytest
import unittest

class Foo(unittest.TestCase):
    # @pytest.mark.parametrize("a", [(1,)])
    def test_foo(self):
        raise Exception("a={}".format(a))
