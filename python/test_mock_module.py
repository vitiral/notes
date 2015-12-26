import unittest
import mock
import os

class TestStuff(unittest.TestCase):
    @mock.patch('os.path', autospec=True)
    def test_mock_module(self, path):
        path.abspath.return_value = "I am patched"
        print(os.path.abspath('hi', 1,2,3))
