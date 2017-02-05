# xml parser src: https://www.bennadel.com/blog/2067-seven-languages-in-seven-weeks-io---day-3.htm

OperatorTable addAssignOperator(":", "atPutNameValue")

XmlDoc := Object clone
XmlDoc init := method(
	self rootNode ::= nil
	self	
)

XmlDoc asString := method(
	self printNode(rootNode, 0)
)

# pretty printed prefix - each level with 2 spaces
XmlDoc getDepthPrefix := method( depth,
	"  " repeated(depth)
)

XmlDoc printNode := method(xmlElement, depth,
	buffer := list()
	newLine := "\n"
	depthPrefix := self getDepthPrefix(depth)

	# append startTag with attributes
	buffer append(
		depthprefix, "<",
		xmlElement name(),
		if((xmlElement attributes size > 0),(
			attributeBuffer := list("") # so first attribute is separated from element
			# convert each key-value-pair to quoted attribute: name="value"
			xmlElement attributes foreach(name, value,
				attributeBuffer append(name .. "=\"" .. value .."\"")
			)
			
			attributeBuffer join(" ")	# 1 line with space delimiter
		),(
			""	# no attributes
		)),
		">",
		newLine
	)

	# append text at depth n+1
	if( xmlElement text,
		buffer append(
			self getDepthPrefix(depth + 1),
			xmlElement txt,
			newLine
		)
	)

	# append each child
	xmlElement childNodes() foreach( childNode,
		buffer append( self printNode(childNode, (depth+1)))
	)

	# append close tag
	buffer append(depthPrefix, "</", xmlElement name(), ">", newLine)

	buffer join("")	
)


# XmlAttributes collection object
# just simpleTypes (number or string) are allowed as value - ignore f.e. list/map!
XmlAttributes := Map clone()
XmlAttributes atPut := method(name, value,
	if(list("Sequence", "Number") contains(value type), # either number or string
		super(atPut(name, value))	# pass to prototype, ignore others
	)
	self
)


# XmlElement each representing one node
XmlElement := Object clone()

XmlElement init := method(
	self name ::= nil	# elementName
	self text ::= nil	# text for simpleType/mixedContent
	self attributes ::= XmlAttributes clone		# empty
	self childNodes ::= list()
	self parentNode ::= nil
	self
)

# XmlParser
XmlParser := Object clone()
## returns new Json-like-Map if attribute notation {name : value} found
XmlParser atPutNameValue := method( name, value,
	("debug name: " .. name .. ", value:" .. value) println 
	attribute := Map clone()
	attribute atPut("name", name)
	attribute atPut("value", value)
	attribute
)
#Map atPutNameValue := method(
	#self atPut(
		#call evalArgAt(0) asMutable removePrefix("\"") removeSuffix("\""),
		#call evalArgAt(1)
	#)
#)

# only in the context of the XmlParser
XmlParser curlyBrackets := method(
	attributes := list()
	#r := Map clone
	#call message arguments println
	call message arguments foreach(nameValuePair,
		#r doMessage(nameValuePair)	
		# evaluating cannot be done in same file as the code which uses it => use doString, not doMsg
		("debug nameValuePair: " .. nameValuePair) println
		("debug nameValuePair asString: " .. nameValuePair asString) println
		("debug doString nameValuePair asString: " .. doString(nameValuePair asString)) println
		attributes.append(
			#doMessage(nameValuePair)
			#self doString( nameValuePair asString)
			self doString( nameValuePair )
		)
	)
	attributes
	#r
)

# all nodes are parsed in context of XmlParser => listen for unknown method calls (no natural slot)
XmlParser forward := method(
	missingMethod := call message name
	missingArgs := call message arguments
	
	xmlElement := XmlElement clone
	xmlElement setName(missingMethod)

	# first arg curly brackets? attribute collection
	if( self isCurlyBrackets( missingArgs at(0)),(

		# remove first curly bracket and process others uniformly
		kvList := missingArgs removeFirst
		attributesList := self doMessage(kvList)
		# list of k-v: "key":"value", "key1":"value1",...
		
		attributesList foreach(attrKV,
			xmlElement attributes atPut(
				attrKV at("name"), attrKV at("value")
			)
		)
	))

	# iterate over remaining args
	missingArgs foreach( argument,
		content := self doMessage(argument) # process arg
		
		if(content type == "XmlElement",(
			content setParentNode(xmlElement)
			xmlElement childNodes append(content)
		),(
			xmlElement setText(content)
		))
	)
	xmlElement
)

XmlParser isCurlyBrackets := method( targetMessage,
	(targetMessage asString findSeq("curlyBrackets")) == 0
)

XmlParser parse := method(
	xmlDoc := XmlDoc clone()
	# xml can only have one root element
	xmlDoc setRootNode(
		self doMessage(
			call message arguments at(0)
		)
	)
	xmlDoc
)

// Parse the XML document.
OperatorTable println
girls := XmlParser parse(
    girls(
        { type: "hotties", isActive: "true" },
        girl(
            { id: 17 },
            name( "Sarah" ),
            age( 35 )
        ),
        girl(
            { id: 104 },
            name( "Joanna" ),
            age( 32 )
        ),
        girl(
            { id: 15 }
        )
    )
);

girls println
