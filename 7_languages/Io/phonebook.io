# sample number file
#  {
#    "Bob Smith": "519555212",
#    "Mary Walsh": "4162223434"
#  }

OperatorTable addAssignOperator(":", "atPutNumber") #add new operator 
# methodname is given: first argument is name (string), second arg a value
# key : value  as   atPutNumber("key", value)

curlyBrackets := method(	# called on curly brackets {}
	r := Map clone
	call message arguments foreach(arg,	# get args from calling msg and iterate through
										# like: list(("key1","value1"),("key2","value2")) foreach
		r doMessage(arg)		# use arg as message: 	r "Bob": "5192"	=> atPutNumber
		)
	r
)

Map atPutNumber := method(	# first arg is somehow quoted string...
	self atPut(
		#call evalArgAt(0), 
		call evalArgAt(0) asMutable removePrefix("\"") removeSuffix("\""),	# Bob
		call evalArgAt(1)													# 5192
	)
)

s := File with("phonenumbers.txt") openForReading contents	# file prototype ...
phoneNumbers := doString(s)		# interpret file content as valid code
phoneNumbers keys println
phoneNumbers values println
