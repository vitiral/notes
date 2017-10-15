# Some questions
https://www.glassdoor.com/Interview/Microsoft-Software-Development-Engineer-Interview-Questions-EI_IE1651.0,9_KO10,39.htm#InterviewReview_17210411

Given root node to a binary tree, reconstruct it and return its root node

## Given 2d matrix and a string. Write a function to check if string is contained in the 2d matrix
Answer:
- brute force method: search each row then each column
- crossword puzzle style: scan for starting character, determine if enough
  space exists right/down and elminate

## Run length encoding, write compression function given input array and output array of bytes
- Design an API for searching for contact information.
- Choose a data structure for search function
- Implement search function

## Given a LARGE list of n unsorted key-value tuples (bigger than one machine)
- Return the k tuples with the greatest value where n is several orders of
  magnitude greater than k
- This list may be dispersed across multiple machines.

My-Answer:
- k tuples DOES fit in memory, and we know how big it needs to be
- First Try: store each tuple in a reverse-BTreeMap (smallest value as root)
  (key=values), constraining the size so that if it goes over the needed size
  you pop the smallest value. If multiple keys need to be found, it would be
  type `value: list[key]`
  - this has performance penalties, each insertion/deletion is O(log n) time,
    but we know that it stays sorted and the root node is always the smallest
    value so it's easy to pop it.
- n is too large to fit in memory, but we could still batch it to improve
  performance. It should be possible to create batches, find it's max/min
  and then merge trees by pruning according to that
