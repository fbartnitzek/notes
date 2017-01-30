# day 3

## meta-programming
- writing programs that write programs
- used in Rails ActiveRecord Framework for building classes that link to db tables
```
class Department < ActiveRecord::Base
  has_many :employees	# ruby methods that add instance variables needed to establish relationship
  has_one :manager
end
```

## Open Classes
- modify existing classes as needed, f.e. to check if string is blank
```
class NilClass
  def blank?
    true
  end
end

class String
  def blank?
    self.size == 0
  end
end

["", "person", nil].each do |element|
  puts element unless element.blank?
end
```
- possible to redefine all f.e. Class.new (probably not a good idea...)
	=> amazing readable code

- units.rb as API expressing distances in inches
	- enhances Numeric (Fixnum => Integer => Numeric)
	- with methods like miles, multiplying itself with 5280.feet
	- and feet which multiplies itself with 12.inches (which returns itself)

- roman.rb
	- use method_missing to convert Roman.XII by name of missing method (not yet perfect)
	- needs strong error checking (accept only roman numerals) 

## Modules

- acts_as_csv_class
  - 2 getter, init, read csv file like class name

- acts_as_csv
  - metaprogramming happens in acts_as_csv-macro (self.acts_as_csv) and defines 4 methods

- act_as_csv_module
  - new RubyCsv instance is created
  - ruby will invoke include whenever module (class) is included (part of declaration)
  - in included method base-class is extended by ClassMethods, which consist only of acts_as_csv
  - that class_method opens up the class and adds multiple instance_methods
  - class that created extended another class, without inheritance
  - important: programs can change, based on the state of the application
	- ActiveRecord uses metaprogramming to dynamically add accessors that are the same name as db-columns
    - XML frameworks like builder let users define custom tags with method_missing => beautiful syntax

## self study
- modify csv app to support an each method to return a CsvRow object
- use method_missing on that CsvRow to return the value for the column for a given heading
- file example
```
one, two
lions, tiger
```
- API example
```
csv = RubyCsv.new
csv.each {|row| puts row.one}	# should print 'lions'
```
- see acts_as_csv_module_each.rb
	- needs extra class with :headers and :row (splitted)
	- and method_missing (without self.)
