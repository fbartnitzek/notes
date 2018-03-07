
/*
 * XML DOM Properties	
 *   x.nodeName - the name of x
 *   x.nodeValue - the value of x
 *   x.parentNode - the parent node of x
 *   x.childNodes - the child nodes of x
 *   x.attributes - the attributes nodes of x
 *
 * XML DOM Methods
 *   x.getElementsByTagName(name) - get all elements with a specified tag name
 *   x.appendChild(node) - insert a child node to x
 *   x.removeChild(node) - remove a child node from x
 *
*/

function parseXmlRoot(node){
	return '' +
		'<div class="panel panel-default">' +
			'<div class="panel-heading">' +
				formatTitle(node) +
			'</div>' +
			formatCollapse(node) +
		'</div>';
}

function formatTitle(node){
	//console.log("formatTitle " + node.nodeName);
	return '' +
		'<h4 class="panel-title">' +
			'<a data-toggle="collapse" href="#collapse-' + calcId(node) + '">' +
				'<span class="glyphicon glyphicon-plus" aria-hidden="true"/>' +
			'</a> ' +
			formatNodeLink(node) +
		'</h4>';
}

function formatNodeLink(node){
		return '<a class="id-links" href="ids/' + calcId(node) + '">' + showNode(node) + '</a>';
}

function formatCollapse(node){
	//console.log("formatCollapse " + node.nodeName);
	var result = '' +
		'<div id="collapse-' + calcId(node) + '" class="panel-collapse collapse">' +
			'<ul class="list-group">';
	
	for (var i=0; i < node.childNodes.length; i++){
		result += '' +
				'<li class="list-group-item">' +
					formatNode(node.childNodes[i]) +
				'</li>';
	}
	
	return result +
			'</ul>' +
			'<div class="panel-footer">End of ' + node.nodeName + '</div>' +
		'</div>';
}

function formatNode(node){
	//console.log("formatNode " + node.nodeName + ", childNodes: " + node.childNodes);
	if (node.childNodes == null || node.childNodes.length == 0){
		return formatNodeLink(node);
	} else {
		return '' +
			formatTitle(node) +
			formatCollapse(node);
	}
}


function calcId(node){
	// TODO!!!
	return node.nodeName;
}

function showNode(node){
	return node.nodeName + showAttributes(node);
}

function showAttributes(node){
	if (node.attributes == null || node.attributes.length == 0){
		return "";
	} else {
		var result = " (";
		for (var i=0; i < node.attributes.length; i++){
			if (i>0){
				result += ", ";
			}
			result += node.attributes[i].nodeName + "=" + node.attributes[i].nodeValue;
		}
		return result + ")";
	}
}

function initXml(){
	return 	"<Root>" +
				"<One></One>" +
				"<Two name='someName'></Two>" +
				"<Three attr='someAttr'>" +
					"<A></A>" +
					"<B>" +
						"<B1 hello='world'>" +
							"<b11></b11>" +
						"</B1>" +
					"</B>" +
					"<C></C>" +
				"</Three>" +
				"<Four/>" +
				"<Five>" +
					"<A2>" + 
						"<X></X>" +
					"</A2>" +
					"<B></B>" +
					"<C></C>" +
				"</Five>" +
				"<Five>" +
					"<A></A>" +
					"<B></B>" +
					"<C></C>" +
				"</Five>" +
			"</Root>";	
}

function init(){
	var xml = initXml();
	var parser = new DOMParser();
	var xmlDoc = parser.parseFromString(xml, "text/xml");
	//console.log(text);

	document.getElementById("demo").innerHTML = parseXmlRoot(xmlDoc.getElementsByTagName("Root")[0]);	
}

init();


/*
//document.getElementById("demo").innerHTML = parseXmlDemo(xmlDoc.getElementsByTagName("Root"));
function parseXmlDemo(list){
	var result = "";
	
	for (var i=0; i < list.length ; i++){
		result += list[i].nodeName + "<br/>";
		//console.log(result);
		var childNodes = list[i].childNodes;
		if (childNodes != null){
			result += parseXml(childNodes);
		}
		
	}
	
	return result;
}
*/