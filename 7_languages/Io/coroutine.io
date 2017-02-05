vizzini := Object clone
vizzini talk := method(
	"vizzini: Fezzik, are ther rocks ahead?" println
	yield
	"vizzini: No more rhymes now, I mean it." println
	yield
)

fezzik := Object clone
fezzik rhyme := method(
	yield
	"fezzik: If there are, we'll all be dead." println
	yield
	"fezzik: Anybody want a peanut?" println
)

vizzini @@talk; fezzik @@rhyme

Coroutine currentCoroutine pause
