There are several approaches for this quite hard problem.
These are very basic methods, and 

1. Generate-and-test

There are lots of candidates
With n characters and length k, there n^k candidates
However, writing a recursive straightforward solution is possible.


2. Optimization

If c does not appear in n strings, it cannot be in the solution.
So, it is possible to eliminate a lot of strings in the candidate generation.
In general, this reduces the number of characters to be taken into account.

3. Optimization using pairs

If pair (a,b) does not appear in n strings with gap constraing g,
it cannot be a part of the solution sequence

This motivates the following:

- Take all pairs that appear in n strings and construct candidate sequences from them
-If (a,b) with gap constraint g appears in n strings, then at least [a,b] is in the solution set
- Further, if (a,b) with gap constraint g appears in n strings, then we may be able to find longer sequences where [a,b] is a subsequence and support is at least n.

Example:

g = 1, required support is 2

abc -> (a,b), (a,c), (b,c)
aaa -> (a,a)
bcc -> (b,c), (c,c)
ac -> (a,c)
bacac -> (b,a), (b,c), (a,c), (a,a), (c,a), (c,c), (a,c)

which pairs have support at least 2?
(a,c)
(b,c)
(a,a)
(c,c)

-> [a,c] [b,c] [a,a] and [c,c] belong to the answer set
-> [b,c,c] is in the third and the last with gap constraint 1

Check the partial solution given for 4.6 and you can continue from there as well...
