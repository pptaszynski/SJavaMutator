package pp.ppscheduler.controller;

import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.util.AbstractMap;
import java.util.ArrayList;

import pp.ppscheduler.model.PPSchedProject;
import pp.ppscheduler.model.PPSchedSet;
import pp.ppscheduler.view.NewProjectWin;

/**
 * Gwny kontroler aplikacji. Kontroluje wybr projektu i po czci bierzcy projekt.
 * Odpowiedzialny za tworzenie projektu.
 * 
 * @author Pawe Ptaszyski
 *
 */
public class PPSchedController extends AbstractController implements WindowListener {
	
	// Pola zestawu projektw, projektu i kontrolera projektu;
	private PPSchedSet projectsSet;
	private PPSchedProject currentProject;
	private PPSchedProjectController currentProjectController;
	
	@Deprecated
	private ArrayList<NewProjectWin> newProjectForms;
	
	/**
	 * Tworzy instancj kontrolera.
	 */
	public PPSchedController(PPSchedSet set) {
		super();
		projectsSet = set;
		
		currentProject = null;
		currentProjectController = null;
	}
	
	/**
	 * Metoda suca do utworzenia pustego projektu o wskazanej nazwie.
	 * 
	 * @param projectName Nazwa projektu do uwtorzenia.
	 */
	public void createProject(String projectName) {
		currentProject = projectsSet.add(new PPSchedProject(projectsSet.getNewId(), projectName) );
		
		currentProjectController = new PPSchedProjectController(this);
		currentProjectController.addModel(currentProject);
	}
	
	/**
	 * Zwraca referencj do aktualnie wybranego projektu
	 * 
	 * @return Aktywny projekt.
	 */
	public PPSchedProject getCurrentProject() {
		return currentProject;
	}
	
	/**
	 * 
	 */
	public ArrayList<AbstractMap.SimpleEntry<Integer, String> > setToList() {
		ArrayList< AbstractMap.SimpleEntry<Integer, String> > setList = new ArrayList< AbstractMap.SimpleEntry<Integer, String> >(); 
		
		Integer idsArray[] = projectsSet.getIdsArr(new Integer[0]);
         
	    try {
	    	for ( Integer id : idsArray ) {
	        	setList.add(new AbstractMap.SimpleEntry<Integer, String>(id, projectsSet.get(id).getName() ) );
	        }
        } catch (Exception e) {
        	System.out.println("Getting projects entries exception");
        	System.exit(-1);
        }
	    
        return setList;
	}
	
	@Deprecated
	public void addProjectForm(NewProjectWin win) {
		newProjectForms.add(win);
	}
	
	@Deprecated
	public void closeNewProjectsForms() {
		for ( NewProjectWin win : newProjectForms ) {
			win.dispose();
			newProjectForms.remove(win);
		}
	}
	
	/**
	 * Metoda pernalizacji.
	 */
	public void applicationClose() {
//		try {
//			projectsSet.writeExternal(null);
//		} catch (Exception e) {
//			
//		}
		
		System.exit(0);
	}
	
	/**
	 * Wygodna metoda  do pozystania kontrolera odpowiedzialnego za obecnie aktywny projekt.
	 * @return Kontroler obecnie aktywnego projektu.
	 */
	public PPSchedProjectController getCurrentProjectController() {
		return this.currentProjectController;
	}

	@Override
	public void windowActivated(WindowEvent e) {}

	@Override
	public void windowClosed(WindowEvent e) {}

	@Override
	public void windowClosing(WindowEvent e) {
		applicationClose();
	}

	@Override
	public void windowDeactivated(WindowEvent e) {}

	@Override
	public void windowDeiconified(WindowEvent e) {}

	@Override
	public void windowIconified(WindowEvent e) {}

	@Override
	public void windowOpened(WindowEvent e) {}

}
