module ActsAsCsv

  def self.included(base)
    base.extend ClassMethods
  end

  module ClassMethods
    def acts_as_csv
      include InstanceMethods
    end
  end



  module InstanceMethods
    attr_accessor :headers, :csv_contents

    def initialize
      read
    end

    def read
      @csv_contents = []
      filename = self.class.to_s.downcase + '.txt'
      file = File.new(filename)
      @headers = file.gets.chomp.split(', ')
      
      file.each do |row|
        @csv_contents << row.chomp.split(', ')
      end
    end


	def each(&block)
	  @csv_contents.each do |row|
	    block.call Row.new(@headers, row)
	  end
	end

  end
end

class Row
  attr_accessor :headers, :row
	def initialize headers, row
	  @headers = headers
	  @row = row
	end

	def method_missing name, *args
		attrName = name.to_s
		i = @headers.index(attrName)
		@row[i] if i.class != NilClass 
	end
end

class RubyCsv # mixed in, no inheritance
  include ActsAsCsv
  acts_as_csv
end

csv = RubyCsv.new
csv.each {|row| puts "hello " + row.one}
csv.each {|line| puts line.two}
csv.each {|row| puts row.three}
