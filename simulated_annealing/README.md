This is an exploration of the optimization process [simulated annealing](https://en.wikipedia.org/wiki/Simulated_annealing). 

The first problem the simulation was tested on is subset sum partitioning. The task is: Given a list of integers, partition them into two
subsets such that the difference between the sums of the two subsets is minimized. For example, given the list (10 15 20 5 25) the best
solution is subsets (sum (10 20 5)) = 35, (sum (15 25)) = 40 for a difference of 5. This is a good problem to test because there are 2^n
possible paritions, so brute force search of all possible solutions becomes inefficient quickly when the list size increases. 
