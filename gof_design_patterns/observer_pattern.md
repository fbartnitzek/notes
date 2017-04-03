# Intro
- src: Java Design Patterns: A Tour of 23 Gang of Four Design Patterns in Java

# Observer Pattern
Define a 1-to-many dependency between objects, so that when 1 object changes state, 
all its dependents are notified and updated automatically.

## Concept
- many observers are observing a particular subject
- observer [un]register themselves to that subject to be notified when there is a change made inside that subject
- a.k.a. Publish-Subscriber model

## Real-Life Example
- celebrity (subject) with many fans (observer)
- fan want to get all the latest updated of their favorite celebrity
- fan can follow celebrity as long as interest persists and can simply stop following if he loses interest

## Computer World Example
- UI (observer) is connected with some database or business logic (subject)
- user can execute some query through UI and after searching the DB the result is reflected back in the UI
- in most cases UI and DB are segregated
- if change occurs in DB, the UI should be notified so that it can update its display according to the change

## Implementation
- ISubject with methods 
	- void register(Observer)
	- void unregister(Observer)
	- void notifyObservers()
- Subject implements ISubject with observerList and methods
	- int getFlag()
	- void setFlag(int)
- Observer with method
	- void update()
	
```
package observer.pattern.demo;
import java.util.*;

class Observer{
	public void update(){
		System.out.println("flag value changed in Subject");
	}
}

interface ISubject{
	void register(Observer o);
	void unregister( Observer o);
	void notifyObservers();
}

class Subject implements ISubject{
	List<Observer> observerList = new ArrayList<Observer>();
	private int _flag;
	public int getFlag(){
	   return _flag;
	}

	public void setFlag(int _flag){
		this._flag=_flag;
		//flag value changed .So notify observer(s)
		notifyObservers();
	}
	@Override
	public void register(Observer o){
		observerList.add(o);
	}
	@Override
	public void unregister(Observer o){
		observerList.remove(o);
	}
	@Override
	public void notifyObservers(){
	   for(int i=0;i<observerList.size();i++){
			observerList.get(i).update();
		}
	}
}
class  ObserverPatternEx{
    public static void main(String[] args){
        System.out.println("***Observer Pattern Demo***\n");
        Observer o1 = new Observer();
        Subject sub1 = new Subject();
        sub1.register(o1);
        System.out.println("Setting Flag = 5 ");
        sub1.setFlag(5);	// output: flag value changed in Subject
        System.out.println("Setting Flag = 25 ");
        sub1.setFlag(25);	// output: flag value changed in Subject
        sub1.unregister(o1);
        //No notification this time to o1 .Since it is unregistered.
        System.out.println("Setting Flag = 50 ");
        sub1.setFlag(50);
    }
}
```

## more complex Example
- multiple observers with different implementations
```
List<IObserver> observersList=new ArrayList<IObserver>();
```
- notify exact change of the subject (myValue is changed in Subject to 5...)
- multiple different Subjects by ISubject with SubjectName
```
public void notifyObservers(int updatedValue){
    for(int i=0;i<observersList.size();i++){
		observersList.get(i).update(this.getClass().getSimpleName(),
		updatedValue);
	}
}
```