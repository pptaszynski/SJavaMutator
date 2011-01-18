package clienteditor;

import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;

public class Client {

    private String firstName;

    private String surname;

    private String nickname;

    private int age;

    private int sex;

    private int maritalStatus;

    private String email;

    private String web;

    private String im;

    private final PropertyChangeSupport changeSupport = new PropertyChangeSupport(this);
    
    public void addPropertyChangeListener(PropertyChangeListener listener) {
        changeSupport.addPropertyChangeListener(listener);
    }

    public void removePropertyChangeListener(PropertyChangeListener listener) {
        changeSupport.removePropertyChangeListener(listener);
    }    
    
    public String getFirstName() {
        return firstName;
    }
    
    public String getSurname() {
        return surname;
    }

    public String getNickname() {
        return nickname;
    }
    
    public int getAge() {
        return age;
    }

    public String getEmail() {
        return email;
    }

    public String getWeb() {
        return web;
    }

    public String getIm() {
        return im;
    }

    public int getSex() {
        return sex;
    }
    
    public int getMaritalStatus() {
        return maritalStatus;
    }

    public void setFirstName(String firstName) {
        String FirstName = this.firstName;
        this.firstName = firstName;
        changeSupport.firePropertyChange("firstName", oldFirstName, firstName);
    }
    
    public void setSurname(String surname) {
        String oldSurname = this.surname;
        this.surname = surname;
        changeSupport.firePropertyChange("surname", oldSurname, surname);
    }

    public void setNickname(String nickname) {
        String oldNickname = this.nickname;
        this.nickname = nickname;
        changeSupport.firePropertyChange("nickname", oldNickname, nickname);
    }

    public void setAge(int age) {
        int oldAge = this.age;
        this.age = age;
        changeSupport.firePropertyChange("age", oldAge, age);
    }

    public void setEmail(String email) {
        String oldEmail = this.email;
        this.email = email;
        changeSupport.firePropertyChange("email", oldEmail, email);
    }

    public void setWeb(String web) {
        String oldWeb = this.web;
        this.web = web;
        changeSupport.firePropertyChange("web", oldWeb, web);
    }

    public void setIm(String im) {
        String oldIm = this.im;
        this.im = im;
        changeSupport.firePropertyChange("im", oldIm, im);
    }

    public void setSex(int sex) {
        int oldSex = this.sex;
        this.sex = sex;
        changeSupport.firePropertyChange("sex", oldSex, sex);
    }
    
    public void setMaritalStatus(int maritalStatus) {
        int oldMaritalStatus = this.maritalStatus;
        this.maritalStatus = maritalStatus;
        changeSupport.firePropertyChange("maritalStatus", oldMaritalStatus, maritalStatus);
    }
  
    public static Client createTestClient() {
        Client client = new Client();
        client.setFirstName("George");
        client.setSurname("Foo");
        client.setNickname("Juraj");
        client.setAge(30);
        
        client.setEmail("g.foo@foo.org");
        client.setWeb("https://beansbinding.dev.java.net");
        client.setIm("ICQ: 53 25 89 76");
        
        client.setSex(1);
        client.setMaritalStatus(2);
		
        return client;
    }
}
