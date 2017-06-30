# Bootstrap

## Intro
- most popular html, css, js framework for developing responsive, mobile first projects on the web
- most forked on github
- just 2 files (css and javascript)
- often used (see expo) creative tim, indicius.com
- good documentation
- help for good looking in few minutes
- already responsive

## Install and use Bootstrap locally / CDN
- download locally and just link bootstrap.css or use online link

## Common Bootstrap components
- access docs if needed
- Jumbotron: lightweight flexible component to highlight sth, takes 100% of parent container
- quick improvement via `<div class="container"> ... </div>` - some whitespace and centered
- forms - also uses 100% of container...
	- `.form-group`! - groups label and input together, so some space to the next entry
	- `.form-control`! - no rounded corners, padding, spacing, other stuff for grid-system
	- `.help-block` - for little help in gray
	- `.form-inline`! - changes multiple-line-form to 1-line-form
	
## NavBar
- `<div class="navbar navbar-default">...</div>` - for gray navbar background
- `<div class="navbar-header"><a href="#" class="navbar-brand">Koffee</a></div>` - for the brand at top left
- `<ul class="nav navbar-nav"><li><a href...>About</a></li>...</ul>` - for the left part of navbar
- `<ul class="nav navbar-nav navbar-right"><li><a href...>Login</a></li>...</ul>` - for the right part of navbar
- `<div class="container">...</div>` - to restrict the content of navbar, but not the navbar itself
- Default NavBar with 'Hamburger-menu'
	- needs bootstrap.js and jqery.js from CDN
	- with `<div class="collapse navbar-collapse" id="bs-nav-demo">...</div>` - so it collapses on some min-size
	- 'hamburger'
```
<div class="navbar-header">
	 <button type="button" class="navbar-toggle collapsed" data-toggle="collapse" data-target="#bs-nav-demo" aria-expanded="false">
		<span class="sr-only">Toggle navigation</span>
		<span class="icon-bar"></span>
		<span class="icon-bar"></span>
		<span class="icon-bar"></span>
	</button>
	<a href="#" class="navbar-brand">Koffee</a>
```
	- 3 bars / lines for hamburger
	- data-target with css-syntax to select collapse-div-id
	- entries outside of collapse-div still stay there
## Grid Layouts
- build out of 12 units (f.e. 50% - 6 units each)
- basic with 2 / 3 columns:
```
<div class="container">
    <div class="row">
        <div class="col-lg-2 pink">COL LG 2</div>
        <div class="col-lg-8 pink">COL LG 8</div>
        <div class="col-lg-2 pink">COL LG 2</div>
    </div>
</div>
```
- after breakpoint all columns use complete width
- 4 different sizes
	- `.col-xs-` - extra small for phones
	- `.col-sm-` - small for tablets
	- `.col-md-` - medium for desktops
	- `.col-xl-` - large for desktops > 1170px
- use multiple unit-sizes for multiple device-sizes, col-md will be used, if col-lg is not given
```
<div class="row">
	<div class="col-md-3 col-sm-6 pink">TOUR DATE!</div>
	<div class="col-md-3 col-sm-6 pink">TOUR DATE!</div>
	<div class="col-md-3 col-sm-6 pink">TOUR DATE!</div>
	<div class="col-md-3 col-sm-6 pink">TOUR DATE!</div>
</div>
```
- nested grids, needs row in row
```
<div class="row">
        <div class="col-md-3 col-sm-6 pink">
            <div class="row">
                <div class="col-lg-6 orange">FIRST HALF</div>
                <div class="col-lg-6 orange">SECOND HALF</div>
            </div>
        </div>
        <div class="col-md-3 col-sm-6 pink">TOUR DATE!</div>
        <div class="col-md-3 col-sm-6 pink">TOUR DATE!</div>
        <div class="col-md-3 col-sm-6 pink">TOUR DATE!</div>
    </div>
```

## Image Gallery
- unsplash pics: enlarge, then copy photo-id from url, use it via
```
<img src="https://source.unsplash.com/cPF2nlWcMY4"/> 
```
- thumbnail class for col-pics
```
<div class="col-lg-4">
	<div class="thumbnail">
		<img src="http://i.imgur.com/qK42fUu.jpg"/>
	</div>
</div>
```
- columns don't need seperate rows (based on 12 - so with 9 x 4 pics, you get 3 rows out-of-the-box)
- use icons/glyphicons with
```
<span class="glyphicon glyphicon-camera" aria-hidden="true">
```
- make navbar fixed to the top when scrolling by adding the class `navbar-fixed-top`
- and move the body a bit to away:
```
body {
    padding-top: 70px;
}
```
- override to specific anchor-tags through "stealing the specific winning selector and overriding it (see inspector)
```
.navbar-inverse .navbar-nav>li>a {
    color: white;
}
```
- different icons with special fonts in fontawesome.io
```
https://www.bootstrapcdn.com/fontawesome/
```
- f.a.q. to pics
```
Here's how you fix the gap on the second row - https://www.udemy.com/the-web-developer-bootcamp/learn/v4/questions/1989586
Here's one way to crop images of varying dimensions to all have matching width and height - codepen.io/nax3t/pen/MJwpdb
And from the docs: if you're looking for Pinterest-like presentation of thumbnails of varying heights and/or widths, you'll need to use a third-party plugin such as Masonry, Isotope, or Salvattore.
```

## Landing Page
- responsive content in the middle of the landing page with
```
#content {
	padding-top: 25%;
}
```
- insert background image, as small for width as possible, centered
```
body {
    background: url(https://source.unsplash.com/xulIYVIbYIc);
    background-size: cover;
    background-position: center;
}
```

- select all h1 and all h3 in css
```
h1, h3 {
    color: white;
}
```

- hr tuning with slight shadow
```
hr {
    width: 400px;
    border-top: 1px solid #f8f8f8;
    border-bottom: 1px solid rgba(0,0,0,0.2);
}
```

- text shadow test (xOffset yOffset blur color [nextShadow])
```
    text-shadow: 0px 4px 3px red,
                 0px 8px 13px orange,
                 0px 18px 23px yellow;
```

- as 3 shades of gray
```
text-shadow: 0px 4px 3px rgba(0,0,0,0.4),
			 0px 8px 13px rgba(0,0,0,0.1),
			 0px 18px 23px rgba(0,0,0,0.1);
```
	
- making bootstrap responsive on mobile devices with the following meta-tag in head above title-tag
```
<meta name="viewport" content="width=device-width, initial-scale=1">
```