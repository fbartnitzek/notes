class Tree
  attr_accessor :children, :node_name

  def initialize(nested={})
	# ruby_tree = Tree.new( "Ruby",
    #   [Tree.new("Reia"),
    #   Tree.new("MacRuby")])

	nested.each do |k,v|
		@node_name = k
		@children = v.map { |k,v| Tree.new(k => v)}
	end

    #@node_name=nested.first[0]	# first key
	#puts @node_name
	#@children = []
	#if nested.values.size > 0
		#nested.values[0].each do |k, v|
			#puts k
			#puts v
			#@children.push(Tree.new(k => v))
		#end
	#end
  end

  def visit_all(&block)
    visit &block
    children.each {|c| c.visit_all &block}
  end

  def visit(&block)
    block.call self
  end
end

ruby_tree = Tree.new({'grandpa' => {'dad' => {'child1' => {}, 'child2' => {}}, 'uncle' => {'child3' => {}, 'child4' => {}}}})

puts "Visiting a node"
ruby_tree.visit {|node| puts node.node_name}
puts

puts "visiting entire tree"
ruby_tree.visit_all{|node| puts node.node_name}
