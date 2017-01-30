linenumber = 0
phrase = ARGV[0]
#phrase = "test"
File.open("testfile.txt", "r") do |infile|
    while (line = infile.gets)
        linenumber = linenumber + 1
		if line.match(phrase)
        	puts "#{linenumber}: #{line}"
		end
    end
end
