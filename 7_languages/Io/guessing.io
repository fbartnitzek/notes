toGuess := Random value (100) ceil
# toGuess println
diff := 100
"guess the number between 1 and 100" println
for (i, 1, 10,
	guess := File standardInput readLine("Enter guess: ") asNumber;
	if (guess == toGuess, "you guessed right!" println; break)
	if ((guess - toGuess) abs < diff,
		"hotter", "colder") println
	diff = (guess - toGuess) abs
)
