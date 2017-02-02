MyList := List clone
MyList dim := method(x,y,
	MyList clone setSize(x) map(x, list() setSize(y)))

MyList set := method(x,y,value,
	self at(x) atPut(y, value))

MyList get := method(x,y,
	self at(x) at(y))

MyList transpose := method(
	# x := self size
	# y := self at(0) size
	# mT := self dim(y, x)
	
	mT := self dim(self at(0) size, self size)
	self foreach(i,l,
		l foreach(j,v,
			# mT println
			mT set(j,i,v)
		)
	)
	mT
)

MyList writeToFile := method(filePath,
	# ("content: " .. self serialized) println
	file := File open(filePath)
	file write(self serialized)
	file close
	#("file " .. filePath .. " written") println
	self 
)


myList := MyList dim(4,3)
myList println

myList set(1,1,"1.")
myList set(1,0,"2.")
myList set(2,2,"3.")
myList println
myList get(1,1) println
myList get(1,0) println
myList get(2,2) println

myList2 := myList transpose
myList2 get(1,1) println
myList2 get(0,1) println
myList2 get(2,2) println

myList writeToFile("./myList.txt")
myList3 := doRelativeFile("./myList.txt")
myList3 println
#myList3 get(2,2) println
