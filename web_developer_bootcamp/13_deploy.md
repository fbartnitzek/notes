# Deploy with Heroku
- signup
- install cli
- init git repo
- initial commit
- heroku create
- remotes created
```
C:\Workspace\tests\DeployingDemo>git remote -v
heroku  https://git.heroku.com/obscure-waters-39352.git (fetch)
heroku  https://git.heroku.com/obscure-waters-39352.git (push)
```
- `git heroku push master`
	- deployed
- application error on site
- see logs: `heroku logs` - not working

- add start script part in package.json with "node app.js"
```
"scripts": {
    "test": "echo \"Error: no test specified\" && exit 1",
	"start": "node app.js"
  },
```
- now working, still no logs...
- https://obscure-waters-39352.herokuapp.com/

## YelpCamp at Heroku
- git init / add / commit
- heroku create
- start script
- git push heroku master
- heroku logs shows logs based on current directory
- run remote commands
	- heroku run ls
- use db: mongolab / www.mlab.com
	- no special chars in username/password
	- single node sand box for free (500MB)
	- dbname: yelpcamp => mongodb://colt:rusty@ds55525.mongolab.com:55525/yelpcamp
	- create db user
- use different databases with env-var `process.env.databaseURL`
	- env-vars in heroku: app - settings - config vars - DATABASEURL = "..."
	- or: `heroku config:set DATABASEURL="..."`