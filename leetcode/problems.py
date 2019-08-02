# https://leetcode.com/problems/next-permutation/
# Next Permuatation
# It wants me to convert a series of numbers into the "next greater
# permutation". If it cannot, it must sort them (lowest possible order)
# 1,2,3 -> 1,3,2
# 3,2,1 -> 1,2,3 (lowest)
# 1,1,5 -> 1,5,1
#
# Okay, I'm slighly confused by what is meant by "next permuatation"
# but I think I might start to be grasping it... it:
# 1. always involves swapping (or moving) only a single number.
# 2. it seems to always move that number only one place over
# 3. it seems like that number must be the largest number that
#    is still to the right.
#
# Okay, assuming I'm right on all those counts. I would solve this
# manually by walking the array and marking the largest value I found
# that was not at the beginning of the array counting down.
#
# Given: 8,7,1,5,1,2,3,4
#
# - I should see 8 then 7 and know that swapping them does not reduce
#   lexigraphical order.
# - I should then see that swapping 1 w/ 7 does not reduce order.
# - I then see that swapping 1 w/ 5 _does_ increase the order
#
# Now, the question asked for the NEXT greater permutation.
# I'm not 100% clear on how to measure this. Either:
# - I swap the first increase in permutation I see (doubtful)
# - OR I can measure the decrease in order, possibly via the diff between the
#   swapped numbers


