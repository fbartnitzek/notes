# RESTful Blog App
- using another css tool
- https://semantic-ui.com/

## Semantic UI
- all
	- `https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.3.0/semantic.min.css`
	- `https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.3.0/semantic.min.js`
	
### simple navbar = menu
```
	<div class="ui fixed inverted menu">
        <div class="ui container">
            <div class="header item">Blog Site</div>
            <a href="/" class="item">Home</a>
            <a href="/blogs/new" class="item">New Post</a>
        </div>
    </div>
```

### simple icons

### css errors on `blogs/new`
- icon smaller
- app.css not a supported file...
- href needs to start with `/` in header.ejs!

### overlapping navbar
- modify container
```
.container.main {
    margin-top: 7.0em;
}
```

### lots of buttons in semantic-ui

### READ ME Link
```
 <a href="/blogs/<%= blog._id%>">Read more</a>
```

### html in blog content
- currently just like usual content in show.ejs:
```
<p><%= blog.body %></p>
```
- change to:
	- html will be executed...
```
<p><%- blog.body %></p>
```
- problem - javascript in content
```
<script>alert("I HAKCED YOU!");</script>
```
- solve with `express-sanitizer`

### limit string on index-page
- `<p><%= blog.body.substring(0, 100) %>...</p>`

## Update and Edit
- fill edit form with value in input and textarea:
```
<input type="text" name="blog[title]" value="<%= blog.title %>">

<textarea name="blog[body]"><%= blog.body %></textarea>
```
- restful update with PUT request
- PUT updates dont work by default, but create long url:
```
<form class="ui form" action="/blogs/<%= blog._id %>" method="PUT">
```
- produces:
```
http://localhost:3000/blogs/5aa2a3546d61a439bc3e7c50?
blog%5Btitle%5D=Test+Blog&
blog%5Bimage%5D=https%3A%2F%2Fcdn.pixabay.com%2Fphoto%2F2018%_720.jpg&
blog%5Bbody%5D=Hello+this+is+a+blog+post%21%0D%0AGettin+tired+of+updates
```
- html forms dont support PUT requests (just POST and GET)
	- found it to difficult... - no inclusive answer
	- https://softwareengineering.stackexchange.com/questions/114156/why-are-there-are-no-put-and-delete-methods-on-html-forms
	- PUT will default to GET-request...
- workaround:
	- Method-Override
```
<form class="ui form" action="/blogs/<%= blog._id %>?_method=PUT" method="POST">
```
	- changes nothing, just a querystring
	- additional package
```
npm install method-override --save
```
	- in app.js
```
...
	methodOverride = require("method-override"),
...
	app.use(methodOverride("_method"));
```
	- SO RESTful routes are not supported in html standard - beautiful standard...
	
- update in app.js `Blog.findByIdAndUpdate(id, newData, callback);`
```
// UPDATE ROUTE
app.put("/blogs/:id", function(req, res){
   Blog.findByIdAndUpdate(req.params.id, req.body.blog, function(err, updatedBlog){
       if(err){
           res.redirect("/blogs");
       } else {
           res.redirect("/blogs/" + req.params.id)
       }
   });
});
```

## DESTROYYY
- app.js:
```
// DELETE ROUTE
app.delete("/blogs/:id", function (req, res) {
    Blog.findByIdAndRemove(req.params.id, function (err) {
        if (err){
            res.redirect("/blogs");
        } else {
            res.redirect("/blogs");
        }
    });
});
```

- links in show:
```
<a class="ui orange basic button" href="/blogs/<%= blog._id %>/edit">Edit</a>
<form id="delete" method="POST" action="/blogs/<%= blog._id %>?_method=DELETE">
	<button class="ui red basic button">Delete</button>
</form>
```
- id just for inline buttons - app.css:
```
#delete {
    display: inline;
}
```

## Final Thoughts
### Sanitize blog body with Express sanitizer
- install in npm
```
npm install express-sanitizer --save
```
- require it in app.js
	- cannot be used before bodyParser
```
    expressSanitizer = require("express-sanitizer"),
	...
	app.use(bodyParser.urlencoded({extended: true}));
	app.use(expressSanitizer());
```

- use it in create and update (when we put data into the database):
```
// CREATE ROUTE
app.post("/blogs", function(req, res){
	// req.body.blog is the blog entry, body is its content-attribute
    req.body.blog.body = req.sanitize(req.body.blog.body);
    Blog.create(req.body.blog, function(err, newBlog){
```
works with simple content:
```
<h1>fsdajlkf</h1>
<script>alert("BAD!!!");</script>
```

### style
- index:
```
<div class="ui main text container">
    <div class="ui huge header">RESTful Blog App</div>
    <div class="ui top attached segment">
        <div class="ui divided items">
            <% blogs.forEach(function(blog){ %>
            <div class="item">
                <div class="image">
                    <img src="<%= blog.image %>">
                </div>
                <div class="content">
                    <a class="header" href="/blogs/<%= blog._id %>"><%= blog.title %></a>
                    <div class="meta">
                        <span><%= blog.created.toDateString() %></span>
                    </div>
                    <div class="description">
                        <%- blog.body.substring(0, 100) %>...
                    </div>
                    <div class="extra">
                        <a class="ui floated basic violet button"
                           href="/blogs/<%= blog._id%>">
                            Read more
                            <i class="right chevron icon"></i>
                        </a>
                    </div>
                </div>
            </div>

            <% }); %>
        </div>
    </div>
</div>
```


### REST table
- see restful_routes.html