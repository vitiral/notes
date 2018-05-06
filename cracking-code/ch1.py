## 1.3: Design  an  algorithm  and  write  code  to  remove  the  duplicate
## characters  in  a  string without using any additional buffer
# Okay, some thoughts:
# - we can't use an additional buffer, so my thought of using a hashset is out.
# - do the characters have to be kept in order? If not, sorting in-place
#   and then removing duplicates is the easiest.
# - if the characters do have to be kept in order, then a brute force
#   solution would be to:
#   - start at character i
#   - scan the string for the same character
#     - remove the character when found
#   - This algorithm is O(n^3)


def remove_dups(s):
    pass



