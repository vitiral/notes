# My notes from "Problem Solving with Algorithms and Data Structures"

## Sorting algorithms:
- bubble sort: basically the worst. n^2 performance. Only advantage is it can
  easily support worst case n performance
- selection sort: you "select" the minimum value by scanning the entire n+1
  array for every n and then swapping it with n. This has very little use
  except as something that is "better than bubble sort".
- insertion sort: natural way to sort playing cards, also n^2 perforamnce. It
  is also a stable sort with O(1) memory requirements. "Insertion" is a
  *really really* bad name, as it does not do *any insertions* (gawd).
  It simply moves values left until they are greater than the value to their
  left. Should be called shift sort...
  - stable
  - More efficient in practice than most other simple quadratic (i.e., O(n^2))
	algorithms such as selection sort or bubble sort
  - Adaptive, i.e., efficient for data sets that are already substantially
	sorted: the time complexity is O(nk) when each element in the input is no
	more than k places away from its sorted position
  - online: can sort a list as it receives it
  - typically the fastest sorting method for small arrays.
    - used internally for quicksort to sort the sub-arrays when the
      arrays are small (typically smaller than 10 elements)
  - can be used directly on linked lists by simply starting with
    an empty list and taking values off of the input list one at a time
    and inserting/shifting them to the correct place.
- mergesort: very simple really.
    - O(n log n) performance but O(n) memory usage
    - divide and conqure: selects the middle and calls mergesort on both
      sides which keeps doing that.
    - when mergesort returns, each side is assumed sorted and they are
      "merged" by moving them one at a time into a new array.
- quicksort:
    - chooses a piviot to divide and conquer, worst case happens when after
      pivioting, the list is not divided in two.
    - simple implementation:
        - piviot is last item, start at beginning and swap and increment
          ref-index for every value < piviot
            - sanity check: if all values are < piviot, they will all have been
              swapped with themselves
            - sanity check: the ref-index will EITHER == count OR equal
              a value >= piviot.
        - finally, swap the ref-index with the piviot-index.

## Hash Tables
Simple hash function: modulo -- just take the `%` of the value

Folding Method:
- divide item into equal size pieces
- add the pieces together

Mid Square Method:
- square the item
- extract some portion of the resulting value (i.e. && 0x0FF0)

# Heap musings

Is a sorted array already a min-heap?

```
[1, 2, 3, 4, 5, 6, 7]


        1
       / \
      2   3
     / \ 6 7
    4   5

```

```
	def percDown(self,i):
		"""percDown recursively perculates the (larger) value at node index=i
		down the 'minimum child' branches.
		'''
		# while it has a left child (and it can only have a right if it has a left)
		while left(i) <= self.currentSize:
			mc = self.minChild(i)  # note: this should be edited to just minKey and return i if that key is itself
			if mc == i:  # then we can add this
				break
			if self.heapList[i] > self.heapList[mc]:
				swap(self.heapList, i, mc)
			i = mc

	def buildHeap(self, alist):
		"""Build a heap from an unsurted list.

		This is done by starting at the lowest node with children
		and calling percDown for every node to the left of it.

		percDown will always find the minimum child and bring it up.
		"""

		i = len(alist) // 2
		self.currentSize = len(alist)
		self.heapList = [0] + alist
		while i > 0:
			self.percDown(i)
			i = i - 1
```

