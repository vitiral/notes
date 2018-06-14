
# QUESTION
https://careercup.com/question?id=5722238880841728

Automation Testing Question:
How do you verify a search result list which changes consistently based on each search word and filters?

For example, how do you make sure that the list is sorted based on price or
rating or etc without any identical list to compare with? Since providing an
identical list as Test Input for each word is not the best approach.


## GIVEN ANSWER
Very open question, I'd cover these aspects:
1. What is purpose of this test? Verify the search function, verify correct
   rendering, ... what is already tested? A single test should fit in some kind
   of test concept which gives an idea on what to test. A reasonable assumption
   could be, that search has been tested already on different known samples and
   information is around how the search has performed compared to this fixed
   set of data.  (notice, there is no perfect search result, but there will be
   indications whether it improved or not compared to other "generaions")
2. Define what must be tested, based on 1) e.g. verify that each items price is
   <= then the next, maybe for the first 20 pages, supposing it is very
   expensive or impossible to fetch all results. How ever, we still could miss
   a very low priced element that would only come after position 1000, or maybe
   even last due to an error. Now, several questions:
  - sometimes search is designed to be fast vs. "perfectly accurate", that means,
    for the case of Google for example, that servers that participate in a query
    and do not respond within a certain time, will be ignored. Usually this is
    the case for not so important pages that have less redundancy etc. etc. So a
    certain error might even be tolerated.
3. Maybe we have statistical data, which gives a price distribution for the
   first 1000 items of a search and information how this distribution changed
   over time. Maybe we only want to accept a certain change in this
   distribution
4. Maybe the test should be around performance, e.g. how fast is the result
   served or how fast is the result rendered. Here we had more information,
   like a SLO (service level objective) where we'd say 99.9% of time the first
   50 result must be served within 100 mS.
5. Or other tests could be that the system accepts "bad searches" which are
   searches that will tear down a server, etc. etc.


# QUESTION
https://careercup.com/question?id=11070934

Google Interview Question for Software Engineer in Tests Software Engineer / Developers
Given an int array which might contain duplicates, find the largest subset of it which form a sequence.
Eg. {1,6,10,4,7,9,5}
then ans is 4,5,6,7

Sorting is an obvious solution. Can this be done in O(n) time

## GIVEN ANSWER
```
"""Given an int array which might contain duplicates, find the largest subset of it which form a sequence.
Eg. {1,6,10,4,7,9,5}
then ans is 4,5,6,7
Sorting is an obvious solution. Can this be done in O(n) time"""
def find(arr):
    table = {}
    first = 0
    last = 0
    for i in arr:
        beg = end = i
        if i in table:
            continue
        table[i] = 'EXISTED'
        if i - 1 in table:
            beg = table[i-1]
        if i + 1 in table:
            end = table[i+1]
        table[beg] = end
        table[end] = beg
        if end - beg > last - first:
            first = beg
            last = end
    return list(range(first, last + 1))

arr = [1,6,10,4,7,9,5, 5,8]

print(find(arr))
```

# QUESTION
https://careercup.com/question?id=5714949223481344

Google Interview Question for Software Engineer in Tests

Design an application which sends the user the 10 closest hotels based on his geo location.
When i asked roughly how many hotels in total are we talking about, i was asked to estimate how many hotels are there in the world...


## GIVEN ANSWER
Here is my take on this problem. I was asked same question in my interview with Goldman Sachs, and the interviewer was satisfied with my answer.

This question is not about algorithm. It is about designing a system(an application). So, it is more than just a algorithm.

First how to organize data. In all the earlier posts people talk about maintaining min heap and tree. Well you cannot load position of all the hotels in the world in you RAM to process for the nearest hotels. You need to persist data in a large database and use DBMS to exploit query stats and abstraction for this. There is no point in loading and organizing data in your RAM(even if you want to do it how would you organize it? 2D Grid? Most of the information is not useful!!! you need some kind of filtering)

Abstraction:Lets say, If a location is around new york(can be decide based on GPS in o(1)) then query the DATABASE VIEW(read sql for what is VIEW) for NEW york. the query would be to calculate distance sqrt(x*x+y*y) (please note: no need of displacement, can be discussed with interviewer). Sort the results and return top 10 from table.

There can be different abstraction on same database or different database for different countries(this is the approach used in big application to organize data on multiple systems)
- MUPala June 25, 2015 | Flag
Reply
0
of 0 votes

EDIT: If further asked about database implementation. Let say you have all the distances how would you find top 10.

use batch for finding top 10, divide the whole data in the set of 20 each(keep the last one of whatever size you have). Find top 10 in each..drop the rest from each group and so on.....
- MUPala July 08, 2015 | Flag
0
of 0 votes

In addition to above:
- Indexes to store the hotels by state, city and/or zipcode will help fast retrieval
- Caching at request nodes will help serve repeated requests from same location
- If load balancing is used, probably distributed caching or global caching helps

# QUESTION
Google Interview Question for Software Engineer in Tests

Given an array of red, green and blue balls arrange them in groups of all red together, greens together and blue together. Do in a single scan of the array.

This is same as You have an array containing only '0's, '1's and '2's. Club same items together in single scan.

# QUESTION
I have a list of several million words unsorted.

How can you find the largest and the smallest words that can be typed by a single hand on a qwerty-style keyboard? Following the rules of finger placement, a word can either be typed fully on the left-hand side of the keyboard, the right-hand side, or both. Find the largest and smallest left-hand word(s), and the largest and smallest right-hand word(s).

given: millions of words, unsorted
given: set of left-hand chars - a,s,d,f,...
given: set of right-hand chars - j,k,l...


## GIVEN ANSWER
1)For all characters in the left hand set add weight = 2.
2) For all characters in the right hand set add weight = 5.
lefthand set is "qwerasdfzxcvtgb "
righthand set is "uiopjklmnhy "
weight[] = {2 or 5 for all of 26 chars};

Foreach word w in dictionary:
1) computer cummulative weight as :
cum_weight = product of weight of all chars in the word.
Eg: cum_weight(cat) = weight[c]*weight[a]*weight[t] = 2*2*2 = 8

2) If (cum_weight % 2 ==0) and (cum_weight %5 !=0)
the word consists of ONLY LEFT HAND set chars.
If (cum_weight %5 ==0) and (cum_weight %2 != 0)
the word consists of ONLY RIGHT HAND set chars.

3) Now u can use max variable to keep track of longest word. Longest word is essentially maximum value in each category.


# QUESTION
https://careercup.com/question?id=13014685

Given a set of non overlapping intervals
Example 1 :(1,4) (6,10) (14, 19) and another interval (13, 17) merge them as (1,4) (6,10) (13,19)

Example 2: (1,5) (6, 15) (20, 21) (23, 26) (27, 30) (35, 40)
New interval (14, 33)
Output should be
(1,5) (6, 33) (35, 40)

This is because the new interval overlaps with (6, 15) (20, 21) (23, 26) (27, 30)

## GIVEN ANSWER

Java implementation

```
public class MergeIntervals {
	class Interval{
		double start;
		double end;
		public Interval(double start, double end) {
			this.start = start;
			this.end = end;
		}
	}

	public List<Interval> mergeIntervals(List<Interval> nonOverlapInt, Interval another){
		List<Interval> merge = new ArrayList<Interval>();
		for(Interval current : nonOverlapInt){
			if(current.end <=  another.start || another.end <= current.start){
				merge.add(current);
			} else{
				another.start = (current.start < another.start) ? current.start : another.start;
				another.end = (current.end > another.end) ? current.end : another.end;
			}
		}
		merge.add(another);
		return merge;
	}
```

# QUESTION
https://careercup.com/question?id=6301845498626048

Design a cache module for an image server. The server accepts image requests
from users and sends them back the images. The cache should always hold
in-memory the 10 most recently requested images. The cache should also support
multiple requests simultaneously


## GIVEN ANSWER
Use a LinkedListHashMap to keep a list of keys ordered by insertion.
Every time you reach the limit (10 in this case), you remove the first inserted key (older one)

```
public static final class BoundedLinkedListHashMap<Key, Value> {
        private LinkedHashMap<Key, Value> map = new LinkedHashMap<>();
        private int N;

        public BoundedLinkedListHashMap(int N) {
            this.N = N;
        }

        public synchronized void put(Key k, Value v) {
            map.put(k, v);

            if (map.keySet().size() > N) {
                removeLast();
            }
        }

        public synchronized Value get(Key k) {
            return map.get(k);
        }

        private void removeLast() {
            map.remove(map.keySet().iterator().next());
        }
    }
```


# QUESTION
https://careercup.com/question?id=5644352850231296

Consider a social network graph like facebook. You are throwing a party and want to invite some of your friends. Design a algorithm to select top n friends from m friends using the social graph.

## GIVEN ANSWER
```
Do Breath First Search of a directed weighted graph where the weight is the closenessWeigth meaning that the higher it is the less close friend relationship is.

This algorithm will get the closest of the closest friends only.

Now I'm thinking about this algorithm when doing a party at my place. ;-)

// This represents a friend
class FriendVertex
{
	// Describes all the edges to another friends
	List<FriendEdge> relationships;

	object Data; // Whatever other data object there is to have.
}

// This represents a friend relationship
class FriendEdge
{
	FriendVertex friend;
	int closenessWeigth;
}

public IEnumerable<FriendVertex> GetNthClosestFriends(
	IEnumerable<FriendVertex> mFriends,
	int nthFriends)
{

	Queue<FriendEdge> q = new Queue<FriendEdge>();

	HashSet<FriendVertex> foundFriends = new HashSet<FriendVertex>();

	// Populate known friends first
	foreach(FriendVertex fv in mFriends)
	{
		// Adding this friends as you don't want then but their friends
		foundFriends.Add(fv);

		q.Enqueue(new FriendEdge() {
				friend = fv,
				closenessWeigth= 0});
	}

	// Stop when there are no friends left or we fill the nthFriendCount
	while(q.Count != 0)
	{
		FriendEdge friendEdge = q.Dequeue();

		if(friend.closenessWeigth != 0)
		{
			friend.closenessWeigth--;
			q.Enqueue(friend);
		}
		else
		{

			// There would definitely be repeated friends so only caring
			// about relationship that friend of a friend first.
			if(!foundFriends.Contains(friend))
			{
				// One less friend needed
				nthFriends--;
				foundFriends.Add(friend);

				yield return friend;
			}

			// This means that we are done finding friends
			if(nthFriends == 0)
				break;

			// Get friends of this friend
			foreach(FriendEdge fe in friend.relationships)
			{
				// Recreating an object to not modify the original from the graph
				q.Enqueue(new FriendEdge() {
						friend = fe.friend,
						closenessWeigth = fe.closenessWeigth});
			}
		}

	}

	// No friends of friends found so returning empty set
	return new List<FriendVertex>();
}
```
