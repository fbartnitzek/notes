# List myAvg := method(self sum / self size)

# use other operator instead of reimplement reduce / sum
# +? should return something like 0 => +!
OperatorTable addOperator("+!", 3)	
Object +! := method(n,
	#if ((n proto) == Number,
	if (n proto == Number and self proto == Number,
		self + n,
		Exception raise("NotANumber!"))
)

List myAvg := method(
	if (self size == 0,
		return(nil)
    )
	self reduce(+!) / 
	self size)

myList := list(1,2,3,4,5,6,7)
myList myAvg println

myEmptyList := list()
myEmptyList myAvg println

#myBadList := list("a","1","2")
#myBadList myAvg println

myBadList2 := list("1","a","2")
myBadList2 myAvg println
