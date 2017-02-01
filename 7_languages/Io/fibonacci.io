# recursion :-)
fib := method(num, if(num <= 2, 1, fib(num-1) + fib(num-2)))

# loop
fib2 := method(num, if(num <= 2, 1,
	current := 0
	last := 1
	prevLast := 1
	for (i, 3, num, current := last + prevLast; prevLast := last; last := current)
	current	
))


for (i, 1, 20, fib(i) println)
for (i, 1, 20, fib2(i) println)
