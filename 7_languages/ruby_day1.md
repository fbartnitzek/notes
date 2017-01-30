# Day 1
- install
- interpreted object-oriented language
- dynamic 'kind-of-strongly-typed' language

## Duck-typing
- string and integer have to_integer-method
- code to an interface works really easy:
```
rb(main):070:0* a = ['100', 100.0]
=> ["100", 100.0]
irb(main):071:0> while i < 2
irb(main):072:1> puts a[i].to_i
irb(main):073:1> i = i + 1
irb(main):074:1> end
100
100
=> nil
irb(main):075:0> 100.methods
=> [:to_s, :inspect, :-@, :+, :-, :*, :/, ..., :integer?, ..., :to_i, :to_int, :floor, :ceil, :truncate, ...
```
## Challenges
- find Ruby in Hello, Ruby
```
str = 'Hello, Ruby'
str.methods.sort
str.index('Ruby')
=> 7
```

- print your name 10 times
```
i=0
irb(main):091:0> (i=i+1; puts 'frank') while i<10
frank
frank
frank
frank
frank
frank
frank
frank
frank
frank
=> nil

```

- print this is sentence number 1 (to 10)
```
irb(main):097:0* i = 0
=> 0
irb(main):098:0> (i=i+1; puts "this is sentence number #{i}") while i<10
this is sentence number 1
this is sentence number 2
this is sentence number 3
this is sentence number 4
this is sentence number 5
this is sentence number 6
this is sentence number 7
this is sentence number 8
this is sentence number 9
this is sentence number 10
=> nil
```

- run ruby from file (see ruby/simple.rb)
```
ruby simple.rb
```

- guess number (see ruby/guess.rb) 


