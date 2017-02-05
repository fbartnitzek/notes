Builder := Object clone
Builder spaces := -1
Builder forward := method(
	self spaces = self spaces + 1
	writeln(printSpaces(spaces), "<", call message name, ">")
	call message arguments foreach(
		arg,
		content := self doMessage(arg);
		if(content type == "Sequence", writeln(printSpaces(spaces+1),content))
	)
	writeln(printSpaces(spaces),"</", call message name, ">")
	self spaces = self spaces - 1
)
Builder printSpaces := method(spaces,
	" " repeated(spaces)
)
Builder ul(
		li("Io"),
		li("Lua"),
		li("JavaScript"))
