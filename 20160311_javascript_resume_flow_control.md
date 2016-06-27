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
- done ... not that interesting

# Functions
- 2 ways to write / define them
	- one
```
var myFunc = function(param1, param2){
	// code here
}
```
	- two
```
function myFunc(param1, param2){
	// code here
}
```

## invoke functions
- simple call without parameters
```
displayWork();
```
- function is using an object that's in the global scope for the script, meaning it's accessible to the function which is also in the global scope

## log clicks
- logClicks function in helper.js
```
clickLocations = [];

function logClicks(x,y) {
  clickLocations.push(
    {
      x: x,
      y: y
    }
  );
  console.log('x location: ' + x + '; y location: ' + y);
}

$(document).click(function(loc) {
  var x = loc.pageX;
		var y = loc.pageY;
		logClicks(x, y);
});
```

## return values
- no return value `var hello = console.log("Hello!");`
- some return values `var myArray = "This is my favorite string!".split(" ");`
- pic
```
	string -acts on--------\
							--> split --return--> array of newly separated strings
	string to --parameter--/
	split on -/
```

- locationizer
```
var work = {
  "jobs": [
    {
      "employer": "Udacity",
      "title": "Course Developer",
      "location": "Mountain View, CA",
      "dates": "Feb 2014 - Current",
      "description": "Who moved my cheese cheesy feet cauliflower cheese. Queso taleggio when the cheese comes out everybody's happy airedale ricotta cheese and wine paneer camembert de normandie. Swiss mozzarella cheese slices feta fromage frais airedale swiss cheesecake. Hard cheese blue castello halloumi parmesan say cheese stinking bishop jarlsberg."
    },
    {
      "employer": "LearnBIG",
      "title": "Software Engineer",
      "location": "Seattle, WA",
      "dates": "May 2013 - Jan 2014",
      "description": "Who moved my cheese cheesy feet cauliflower cheese. Queso taleggio when the cheese comes out everybody's happy airedale ricotta cheese and wine paneer camembert de normandie. Swiss mozzarella cheese slices feta fromage frais airedale swiss cheesecake. Hard cheese blue castello halloumi parmesan say cheese stinking bishop jarlsberg."
    },
    {
      "employer": "LEAD Academy Charter High School",
      "title": "Science Teacher",
      "location": "Nashville, TN",
      "dates": "Jul 2012 - May 2013",
      "description": "Who moved my cheese cheesy feet cauliflower cheese. Queso taleggio when the cheese comes out everybody's happy airedale ricotta cheese and wine paneer camembert de normandie. Swiss mozzarella cheese slices feta fromage frais airedale swiss cheesecake. Hard cheese blue castello halloumi parmesan say cheese stinking bishop jarlsberg."
    },
    {
      "employer": "Stratford High School",
      "title": "Science Teacher",
      "location": "Nashville, TN",
      "dates": "Jun 2009 - Jun 2012",
      "description": "Who moved my cheese cheesy feet cauliflower cheese. Queso taleggio when the cheese comes out everybody's happy airedale ricotta cheese and wine paneer camembert de normandie. Swiss mozzarella cheese slices feta fromage frais airedale swiss cheesecake. Hard cheese blue castello halloumi parmesan say cheese stinking bishop jarlsberg."
    }
  ]
};

// Your code goes here! Let me help you get started

function locationizer(work_obj) {
    var locations = [];
    for (jobIndex in work_obj.jobs){
        var job = work_obj.jobs[jobIndex];
        locations.push(job.location);
    }
    
    return locations;
}

// Did locationizer() work? This line will tell you!
console.log(locationizer(work));
```

## international name
```
/*
The International Name challenge in Lesson 2 where you'll create a function that will need this helper code to run. Don't delete! It hooks up your code to the button you'll be appending.
*/
$(document).ready(function() {
  $('button').click(function() {
    var iName = inName() || function(){};
    $('#name').html(iName);  
  });
});

function inName(){
	var name = $("#name").text().trim();
	var firstSpacePosition = name.indexOf(" ");
	console.log(firstSpacePosition);
	var firstName = name.substr(0, firstSpacePosition);
	firstName = firstName.slice(0,1).toUpperCase() 
				+ firstName.slice(1).toLowerCase();
	var rest = name.substr(firstSpacePosition + 1);
	console.log(rest);
	return firstName + " " + rest.toUpperCase();
};
```