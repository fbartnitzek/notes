# Day 2

## functions 
	- get interpreted on runtime
```
irb(main):001:0> def tell_the_truth
irb(main):002:1>   true
irb(main):003:1> end
=> :tell_the_truth
```

## arrays
```
irb(main):005:0> animals = ['lions', 'tigers', 'bears']
=> ["lions", "tigers", "bears"]
irb(main):007:0> puts animals.sort if tell_the_truth
bears
lions
tigers
=> nil

irb(main):008:0> animals[1]
=> "tigers"
irb(main):009:0> animals[-2]
=> "tigers"
irb(main):010:0> animals[10]
=> nil
irb(main):012:0> animals[0..1]
=> ["lions", "tigers"]
irb(main):013:0> (0..1).class
=> Range

irb(main):007:0> [1].methods.include?(:[])
=> true
```

- use arrays as queue, linked list, stack or set (with push/pop, multidimensional, ...)

## hashes
- every object has a label

```
irb(main):020:0> numbers = {1 => 'one', 2 => 'two'}
=> {1=>"one", 2=>"two"}
irb(main):021:0> numbers[2]
=> "two"
irb(main):022:0> stuff = {:array => [1,2,3], :string => 'Hi, mom!'}
=> {:array=>[1, 2, 3], :string=>"Hi, mom!"}
irb(main):023:0> stuff[:string]
=> "Hi, mom!"
```

- ':symbol' identifier preceded with a colon -> great for naming things
- same string value for different keys possible
- identical symbols are the same object

```
irb(main):024:0> 'string'.object_id
=> 69926396612160
irb(main):025:0> 'string'.object_id
=> 69926396602260
irb(main):026:0> :string.object_id
=> 267228
irb(main):027:0> :string.object_id
=> 267228

```

- no named parameter support, workaround with hashes
```
irb(main):033:0> def tell_the_truth(options={})
irb(main):034:1>   if options[:profession] == :lawyer
irb(main):035:2>     'it could be believed that this is almost certainly not false.'
irb(main):036:2>   else
irb(main):037:2*     true
irb(main):038:2>   end
irb(main):039:1> end
=> :tell_the_truth
irb(main):040:0> tell_the_truth
=> true
irb(main):041:0> tell_the_truth( :profession => :lawyer )	//braces optional for last parameter
=> "it could be believed that this is almost certainly not false."

```

## Code block and Yield
```
irb(main):042:0> 10.times {puts 'frank'}
```
- code block is between braces
- times is a method of Fixnum
- specify code blocks with `{/}` or `do/end`

```
irb(main):043:0> animals = ['lions and ', 'tigers and', 'bears', 'oh my']
=> ["lions and ", "tigers and", "bears", "oh my"]
irb(main):044:0> animals.each {|a| puts a}
lions and 
tigers and
bears
oh my
```

- custom implementation of times
  - adds method to existing class
```
irb(main):045:0> class Fixnum
irb(main):046:1>   def my_times
irb(main):047:2>     i = self
irb(main):048:2>     while i > 0
irb(main):049:3>       i = i - 1
irb(main):050:3>       yield
irb(main):051:3>     end
irb(main):052:2>   end
irb(main):053:1> end
=> :my_times
irb(main):054:0> 3.my_times {puts 'mangy moose'}
mangy moose
mangy moose
mangy moose
=> nil
```

- blocks as first-class parameters
```
irb(main):065:0* def pass_block(&block)
irb(main):066:1>   call_block(&block)
irb(main):067:1> end

irb(main):069:0> def call_block(&block)
irb(main):070:1>   puts 'from within call_block'
irb(main):071:1>   block.call
irb(main):072:1> end

irb(main):073:0> pass_block {puts 'Hello, block'}
from within call_block
Hello, block
=> nil
```

- use it to delay execution
```
execute_at_noon {puts 'time to get up'}
```

- conditionally execute something
```
def in_case_of_emergency
  yield if emergency?
end
```

- enforce policy 
```
def within_a_transaction
  begin_transaction
  yield
  end_transaction
end
```

## classes and trees (see tree.rb)
- conventions
	- classes in UpperCamelCase
	- @instance_variables (1 per object)
	- @@class_variables (1 per class)
	- instance_variables in underscore_style, Constants in ALL_CAPS
	- functions and methods that test typically use a question mark (if test?)
	- attr keyword defines an instance variable
		- attr defines an instance variable and a method of the same name to access it
		- attr_accessor - defining an instance variable (an accessor) and a setter

## Mixin / Modules
- see example to_file.rb
- extraordinary: method to_s is used in module, but implemented in class (hidden abstract method...)
	- java: contract is explict => class will implement a formal interface
	- ruby: contract is implicit => duck typing
- mixing in the ability to write the content of the class to a file
- add new mixins and subclasses to Person possible => all subclasses have capabilities of all the mixins (w/o knowing impl)
- summary: simplified single inheritance to define the essence of a class, then attach additional capabilities with modules

## Modules, Enumberable and Sets

```
// spaceship implemented on strings
irb(main):083:0> 'begin' <=> 'end'
=> -1
irb(main):084:0> 'same' <=> 'same'
=> 0
irb(main):085:0> a = [5, 3, 4, 1]
=> [5, 3, 4, 1]
irb(main):086:0> a.sort
=> [1, 3, 4, 5]
irb(main):087:0> a.any?
=> true
// spaceship implemented on Fixnum => sort, min, max
irb(main):088:0> a.any? {|i| i > 5}
=> false
irb(main):089:0> a.any? {|i| i > 4}
=> true
irb(main):090:0> a.all? {|i| i > 4}
=> false
irb(main):091:0> a.all? {|i| i > 0}
=> true
// set-based operations collect and map
irb(main):092:0> a.collect {|i| i *2}
=> [10, 6, 8, 2]
irb(main):093:0> a.select {|i| i % 2 == 1}
=> [5, 3, 1]
irb(main):094:0> a.max
=> 5
irb(main):096:0> a.member? (2)
=> false

// inject magic
irb(main):097:0> a.inject(0) {|sum, i| sum+i}
=> 13
irb(main):098:0> a.inject {|sum, i| sum+i}
=> 13
irb(main):099:0> a.inject {|product, i| product*i}
=> 60

```

-injection: codeblock with 2 arguments
	- executed for each item in list (2nd argument)
	- first arg is execution of the previous execution of the code block
	- either first arg is explicitly given or the first item in list is used as init value and iteration starts at second item

## Self study
- Find out how to access files with and without code blocks. What is the benefit of the code block?
	- without block: explicit error handling and closing of file
```
newFile = File.open('test.txt','w')
newFile << 'Some contains'
newFile.close
```
	
	- with block: kind of try-with-resource-style with autoclose, can return in any way, documented
```
File.open('test.txt','w') do |f|
  f << 'Some contains'
end
```

- How would you translate a hash to an array? Can you translate arrays to hashes?
	- arrays to hash is simple => key=index, value=value
```
irb(main):112:0> hash = {}
=> {}
irb(main):113:0> a
=> [1, 2, 3, 4]
irb(main):114:0> a.each_with_index {|item,index| hash[index] = item}
=> [1, 2, 3, 4]
irb(main):115:0> hash == a
=> false
irb(main):116:0> hash[1] == a[1]
=> true
```

	- hash to array - a bit useless		AND can you iterate through a hash
```
irb(main):119:0* keys = []
=> []
irb(main):120:0> values = []
=> []
irb(main):121:0> pairs = []
=> []
irb(main):122:0> hash.each{|key, value| keys.push(key)}
=> {0=>1, 1=>2, 2=>3, 3=>4}
irb(main):123:0> hash.each{|key, value| values.push(value)}
=> {0=>1, 1=>2, 2=>3, 3=>4}
irb(main):126:0> hash.each{|key, value| pairs.push([key,value])}
=> {0=>1, 1=>2, 2=>3, 3=>4}
irb(main):124:0> keys
=> [0, 1, 2, 3]
irb(main):125:0> values
=> [1, 2, 3, 4]
irb(main):127:0> pairs
=> [[0, 1], [1, 2], [2, 3], [3, 4]]
```

- how can you use ruby arrays, f.e. stacks?
	- stack: push and pop
	- set: uniq
	- map
	- inject
	- linked list: next

## Challenge
- print the content of an array of 16 numbers, 4 numbers at a time, using just each; then do the same using each_slice
```
irb(main):190:0* idx = 0
=> 0
irb(main):191:0> (1..16).each do |x|
irb(main):192:1*   print x
irb(main):193:1>   ++idx
irb(main):194:1>   puts if x % 4 == 0
irb(main):195:1> end
1234
5678
9101112
13141516
=> 1..16

// or
rb(main):197:0> (1..16).each_slice(4) {|x| p x}
[1, 2, 3, 4]
[5, 6, 7, 8]
[9, 10, 11, 12]
[13, 14, 15, 16]
=> nil
```


- modified tree example: let the init accept a nestes structures of hashes, f.e.
```
{'grandpa' => {'dad' => {'child1' => {}, 'child2' => {}}, 'uncle' => {'child3' => {}, 'child4' => {}}}}
```
	- see tree2.rb

	- write a simple grep that will print the lines of a file having any occurences of a phrase anywhere in that line
		- simple regex and read lines from file
		- optionally line numbers
		- see file.rb (quite easy :-))
