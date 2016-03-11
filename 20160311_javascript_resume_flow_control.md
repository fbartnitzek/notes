javascript basics resume - Flow Control
=======================================
udacity course: 
```
https://www.udacity.com/course/viewer#!/c-ud804/l-2239648539/e-1945908556/m-1928448581
```

Evaluators
----------
- to compare expressions
- `<, >, <=, >=, ===, !=`
- strict equality `===`
	- no type conversion
	- equal value and equal type
- loose equality `==`
	- type conversion if different type
	- equal value?
- link: <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Equality_comparisons_and_sameness>
- same syntax as java
```
if (condition){
	doSomething();
} else {
	doSomethingElse();
}
```

### QUIZ
append skills if not empty
```
if (bio.skills.length > 0){
	$("#header").append(HTMLskillsStart);
	
	var formattedSkill = HTMLskills.replace("%data%", bio.skills[0]);
	$("#skills").append(formattedSkill);
	
	formattedSkill = HTMLskills.replace("%data%", bio.skills[1]);
	$("#skills").append(formattedSkill);
	
	// ... more skills
}
```

## while loops
```
while(condition){
	doSomething();
}
```

sample:

```
var cameron = {};
cameron.job = "course dev";

var makeCourse = function() {
	// make a course
	console.log("Made a course");
}

var courses = 0;
while(cameron.job==="course dev"){
	makeCourse();
	courses = courses + 1;
	if(courses === 10){
		cameron.job = "learning specialist";
	}
}

console.log(cameron.job);
```
works :-)

## for loops
```
for (initialization;condition;mutator){
	doSomething);
}
```

like:

```
for (var i=0;i<9;i++){
	console.log(i);
}
```

## for in loop
```
var countries = ["Germany", "Argentina", "Brazil", "Netherlands"];

for (country in countries){	// index instead of object in array
	// console.log(country);	//returns id...
	console.log(countries[country]);	//returns value
}
```

### QUIZ - append jobs
```
for (job in work.jobs){
		$("#workExperience").append(HTMLworkStart);
		
		var formattedEmployer = HTMLworkEmployer.replace("%data%", work.jobs[job].employer);
		var formattedTitle = HTMLworkTitle.replace("%data%", work.jobs[job].title);
		var formattedEmployerTitle = formattedEmployer + formattedTitle;
		
		$(".work-entry:last").append(formattedEmployerTitle);	
		// in jquery we're selecting every object with a class work-entry	
		// class because it has a dot in front of it
		// and append variable to the last element on the page (the newly created one)
	}
```

### QUIZ - remaining strings in jobs with matching template data ...
```
TODO!
```