"""

- There may or may not be a "celebrity" at the party who is known by everyone.
- However, if they are there they don't know anyone.
- Find the celebrity using the funciton HaveAquaintence(A, B) in the minimum
  number of questions.


First thought: simple brute force is to select person A and first prove they
are not the celebrity by finding a single person who they know / doesn't know
them.

Once you know they are NOT the celibrity, loop through people who:
- they know
- don't know them.

Find the set of all these people and do the same query within them. The
celebrity guarantee will still hold for them.

This will reduce the list to only a few people (maybe even 1 or none). Do a
complete check on any who are left (possibly using cached results if calling
the HaveAquaintence function was expensive).
"""
