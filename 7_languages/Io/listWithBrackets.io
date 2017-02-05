squareBrackets := method(
	l := list()
	call message arguments foreach(arg,
		l push(call sender doMessage(arg))
	)
	l
)

#listExample := doString("['hello','world','example']")
s := File with("listExample.txt") openForReading contents
listExample := doString(s)
listExample foreach(v, v println)
