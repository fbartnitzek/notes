# Web Auth from Scratch
- https://www.youtube.com/watch?v=i7of02icPyQ
- gup as new html-format (like ejs)
## Cockie
- just a value in the header:
```
{
...
	"Set-Cookie": "session=12345"
}
```
- usage in express:
- client-sessions by mozilla: `npm install client-sessions`
- app.js:
```
const session = require("client-sessions");
app.use(sessions({
	cookieName: "session",
	secret: "woosafd32532wfsf",
	duration: 30 * 60 * 1000	// 30mins
}));
```
- using sessions
```
app.post("/login", (req, res) => {
	User.findOne({email: req.body.email }, (err, user) => {
		// unsafe
		if(!user || req.body.password !== user.password) {
			return res.render("login", {error: "Incorrect email / password."});
		}
	}
	req.session.userId = user._id;
	res.redirect("/dashboard");
});

app.get("/dashboard", (req, res, next) => {
	if(!(req.session && req.session.userId)) {
		return res.redirect("/login");
	}
	User.findById(req.session.userId, (err, user) => {
		if (err){
			return next(err);
		}
		if (!user){
			return res.redirect("/login");
		}
		res.render("dashboard");
	});
});
```

## Hashing
- Password Hashing
	- same password always generates the same hash
	- given a hash, you can never retrieve the original password that created it. Hashes are one-way
- Hashing methods / libraries
	- bad: md5, sha256
	- secure default: bcrypt (safe, old, good) - cpu-usage
	- better: scrypt (2006) - cpu- and memory-usage
	- gold-standard: argon2 (new) - cpu- and memory- and physical cpu-usage 
	- exponential performance differences
- using bcrypt
	- `npm install bcryptjs`
	- app.js:
```
app.post("/register", (req, res) => {
	// 14 is bcrypt-work-factor (how strong should hash be, higher is stronger)
	let hash = bcrypt.hashSync(req.body.password, 14);
	req.body.password = hash;
	let user = new User(req.body);
	// ...
});

app.post("/login", (req, res) => {
	User.findOne({email: req.body.email }, (err, user) => {
		if (!user || !bcrypt.compareSync(req.body.password, user.password)) {
			return res.render("login", {error: "incorrect email / password"});
		}
		// ....
	});
});
```

- http://plaintextoffenders.com

## Smart User Middleware
```
app.use((req, res, next) => {
	if (!(req.session && req.session.userId)){
		return next();
	}
	
	User.findById(req.session.userId, (err, user) => {
		if(err) {
			return next(err);
		}
		
		if (!user) {
			return next();
		}
		
		user.password = undefined;	// override password-hash to nothing
		req.user = user;	// later on print firstname
		res.locals.user = user;	// express-specific: user-variable in each template
		
		next();
	});
});
```

## Force Authentication
```
function loginRequired(req, res, next){
	if (!req.user){
		return res.redirect("/login");
	}
	next();
});
```

## CSRF
- cross side request forgery
- example
	- bank with widthdraw
```
<form>
	<input type="text" name="account"/>
	<input type="text" name="amount"/>
	<input type="text" name="for"/>
</form>
```

- email:
```
Hey Randall,
check out this pic of my dog!

<img src="http://bank.com/withdraw?account=Randall&amp;amount=1000000&amp;for=BadGuy">
```
- CSRF Tokens
	- hidden input field in login-field
	- in response stored in cookie
	- POST contains CT, same like in login? => valid, else=> reject
- html:
```
<form method="post">
	<input type="hidden" name="_csrf", value=csrfToken>
</form>
```
- app
	- `npm install csurf`
	- usage:
```
const csurf = require("csurf");

app.use(csurf());

app.get("/register", (req, res) => {
	res.render("register", {csrfToken: req.csrfToken() });
});

app.get("/login", (req, res) => {
	res.render("login", {csrfToken: req.csrfToken() });
});
```

## Security Best Practice
- always use SSL
- use Cookie Flags
```
app.use(session({
	cookieName: 'session',
	secret: 'some_random_string',
	duration: 30 * 60 * 1000,
	activeDuration: 5 * 60 * 1000,
	httpOnly: true,		// don't let JS code access cookies
	secure: true,		// only set cookies over https
	ephemeral: true		// destroy cookies when the browser closes
}));
```
- wear a helmet
	- `npm install helmet`
	- does lots of http header security stuff - see docs
	- usage:
```
const helmet = require("helmet");

// manage http header security
app.use(helmet());
```
- don't roll your own
	- passport
	- node-login
	- aqua
	- okta