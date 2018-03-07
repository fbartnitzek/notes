function average(scores){
	return Math.round(recSum(scores) / scores.length);
}

function recSum(scores){
	return scores.length === 1 ? scores[0] : (scores[0] + recSum(scores.slice(1)))
}

function average2(scores){
	var sum=0;
	scores.forEach(function(score){
		sum+=score;
	});
	
	return Math.round(sum / scores.length);
}

function average3(scores){
	return Math.round(scores.reduce((sum, val) => sum + val) / scores.length);
}



//console.log([5,15,25].slice(1));
//console.log(recSum([5,15]));

var scores= [90, 98, 89, 100, 100, 86, 94];
console.log(average(scores));	// 94
console.log(average2(scores));	// 94
console.log(average3(scores));	// 94
var scores2= [40, 65, 77, 82, 80, 54, 73, 63, 95, 49];
console.log(average(scores2));	// 68
console.log(average2(scores2));	// 68
console.log(average3(scores2));	// 68