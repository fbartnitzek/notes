OperatorTable addAssignOperator(":", "atPutNumber")

curlyBrackets := method(
    m := Map clone
    call message arguments foreach(arg, 
        m doMessage(arg))
    m
)

squareBrackets := method(
	l := list()
	call message arguments foreach(arg,
		l push(call sender doMessage(arg)))
	l
)

Map atPutNumber := method(  # first arg is somehow quoted string...
    self atPut(
        call evalArgAt(0) asMutable removePrefix("\"") removeSuffix("\""),  # Bob
        call evalArgAt(1)                                                   # 5192
    )
)


#listExample := doString("['hello','world','example']")
s := File with("json.txt") openForReading contents
json := doString(s)
json at("glossary") at("title") println
json at("glossary") at("GlossDiv") at("GlossList") at("GlossEntry") at("GlossDef") keys println
json at("glossary") at("GlossDiv") at("GlossList") at("GlossEntry") at("GlossDef") values println

# TODO: why does this not work as expected...?
#json at("glossary") at("GlossDiv") at("GlossList") at("GlossEntry") at("GlossDef") foreach(v, println(v))
