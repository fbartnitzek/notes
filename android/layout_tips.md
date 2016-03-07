# layout_tips
absolute layout does not work for multiple devices (tv - wearable - tables - smartphone)

- other:
	- FrameLayout
		- for simple Layouts with a singleView (f.e. listView) / stack or views - aligned against frame boundaries only
	- LinearLayout
		- stacking views vertically or horizontally (one after another)
	- RelativLayout
		- allowss positioning of views relative to other views or the boundaries of the view (parent)
	- GridLayout

- Responsive Design!

scrollable list with 10 items 
	- needs 12 items, 2 recycling views 
	- called AdapterView (ListView, GridView)
