
matrix := list(list(1,2,3),list(4,5,6),list(7,8,9,10,11))
mysum := 0
for (i, 1, matrix size, mysum := mysum + matrix pop sum)
mysum println

# with reduce - matrix seems to be 'used'
matrix := list(list(1,2,3),list(4,5,6),list(7,8,9,10,11))
matrix reduce(s, x, s + x sum, 0) println
