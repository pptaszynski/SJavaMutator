package clienteditor;

import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;

public class Client {
	
	private String firstName;
	
	private final PropertyChangeSupport changeSupport = new PropertyChangeSupport(this);
	
	public void addPropertyChangeListener(PropertyChangeListener listener) {
        changeSupport.addPropertyChangeListener(listener);
    }
	
	public String getSurname() {
        return surname;
    }
}