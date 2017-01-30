
val = rand(10)
puts 'guess the number 0 - 9'

gv = -1
begin gv != val
	gv = gets.to_i
	puts "you guessed right: #{val}" if gv == val
	puts "you guessed to low" if gv < val
	puts "you guessed to high" if gv > val
end while gv != val
