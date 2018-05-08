"""PG-49: Stacks and Queues"""

class Node(object):
    def __init__(self, val, next=None):
        self.val = val
        self.next = next


class Stack(object):
    """A stack is a first in last out buffer.

    It can be easily implemented as an array that you
    - append to for push
    - pop from for pop.

    In this case we implement it as a linked list where
    any pushed item always becomes the "top"

    """
    def __init__(self):
        self.top = None

    def push(self, val):
        self.top = Node(val, next=self.top)

    def pop(self, val):
        out = self.top
        if out:
            self.top = out.next
        return out



def Queue(object):
    """A queue is a first in first out buffer.

    It can be implemented using a circular buffer. Here we implement it as a
    linked list.
    """
    def __init__(self):
        self.first = None
        self.last = None

    def push(self, val):
        node = Node(val)
        if self.first:
            assert self.last
            node.next = self.last
            self.last = node
        else:
            # it is the first node to be inserted
            assert self.first is None
            self.last = self.first = node

    def pop(self):
        if not self.first:
            raise IndexError("Queue is empty")

        out = self.first
        self.first = out.next
        if self.first is None:
            self.last = None
        return out


################################################################################
# PG-50: 3.1 Describe how you could use a single array to implement three stacks
# Two is very easy: the left and the right of the array and grow towards the
# middle.
#
# 3 is harder for sure. There are a couple of options:
# - Split the array up into 3 even slices (non-optimal, wasted memory)
# - Have "pointers" to the index of the next element alongside the values.
#   this actually allows you to have N stacks. You can also store a "free
#   linked list" within the array.
# - "grow out" the middle. Things will still become space constrained though.
