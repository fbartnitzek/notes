# App Basics

## application
- loose collection of classes for the user to interact with
- ui component organized into activities
- classes	
	- content provider - manage app data
	- services - run background tasks without ui (download, music)
	- broadcast receivers - listen/respond to system announcements (screen on / network connectivity)

## activity
- for user interactions
- initial activity - default MainActivity
- single, focused thing that user can do

## fragment
- activities >1 fragments
- modular sections of an activity (usualy meant to display ui)
- placeholderFragment automatically generated as inner class of activity (dont need to be inner class)

## view and viewGroup
- basic building block for ui components
- fragment may contain multiple views to design its layout
- subclasses: buttons, text, other widgets - can be combined in viewGroups
- common viewGroups: LinearLayout, RelativeLayout, FrameLayout

## XML Layout
- defines collection of views, viewGroups and relationships between them

	1) inflate: xml layout to java view

	2) associate: inflated layout with activity or fragment

	a) for an activity (onCreate)

```
setContentView(R.layout.activty_main);
```

	b) for a fragment (onCreateView)

```
View rootView = inflater.inflate(R.layout.fragment_main, container, false);
...
```

##ListView
- subclass of view
- optimized for displaying lists by displaying copies of single layout
- populated by adapter

## adapter
- translates data source into views for a listview to display
