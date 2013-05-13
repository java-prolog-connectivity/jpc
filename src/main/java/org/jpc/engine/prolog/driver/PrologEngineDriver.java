package org.jpc.engine.prolog.driver;

import java.util.Collection;

import org.jpc.JpcPreferences;
import org.jpc.engine.listener.DriverStateListener;
import org.jpc.engine.prolog.PrologEngine;
import org.jpc.engine.prolog.PrologEngineInitializationException;
import org.jpc.util.naming.Nameable;
import org.minitoolbox.CollectionsUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public abstract class PrologEngineDriver<T extends PrologEngine> implements PrologEngineFactory<T>, Nameable {

	private static Logger logger = LoggerFactory.getLogger(PrologEngineDriver.class);
	private String name; //This optional attribute is intended to be used for GUI development in a multi-engine environment.
	private JpcPreferences preferences;

	private Collection<DriverStateListener> listeners;
	
	public PrologEngineDriver() {
		this(new JpcPreferences());
	}
	
	/**
	 * A PrologDriver may be instantiated with incomplete information. 
	 * This is with the intention to allow this data to be added later (e.g., with a GUI)
	 * @param preferences
	 */
	public PrologEngineDriver(JpcPreferences preferences) {
		this.preferences = preferences;
		/*
		 * Using a weak set for keeping the list of state listeners, so references to these listeners can be collected by the GC if required.
		 */
		listeners = CollectionsUtil.createWeakSet();
		String defaultConfigurationName = getShortDescription();
		setName(defaultConfigurationName);
	}


	public JpcPreferences getPreferences() {
		return preferences;
	}

	@Override
	public String getName() {
		return name;
	}

	@Override
	public void setName(String name) {
		this.name = name;
	}

	/**
	 * Answers if the driver is disabled (cannot create a Prolog Engine session) or enabled.
	 * Normally this property cannot be set by the programmer but depends on the internals of the concrete driver implementation (that is the reason is not implemented as an instance variable).
	 * The original motivation of this property is to enable GUI tools to show properly the driver description and availability on the screen
	 * Normally, drivers supporting multiple Prolog engines are always enable.
	 * However, drivers supporting only one Prolog session are enabled only before creating the first (and unique) Prolog session, and disabled afterwards.
	 * Note that certain libraries (e.g., JPL) allows only one session using the library, so it is not possible to start a second session with a different Prolog engine that uses the same library.
	 * For example, if a SWI Prolog engine is created using JPL, it is not going to be possible to create a YAP Prolog engine using also JPL.
	 * @return the disabled state of the driver
	 */
	@Override
	public boolean isDisabled() {
		return false;
	}


	@Override
	public T createPrologEngine() {
		if(isDisabled())
			throw new PrologEngineInitializationException("The driver cannot instantiate new Prolog engines.");
		readyOrThrow();
		logger.info("Initializing logic engine ...");
		long startTime = System.nanoTime();

		T newPrologEngine = basicCreatePrologEngine();
		onCreate(newPrologEngine);
		newPrologEngine.flushOutput();
		
		long endTime = System.nanoTime();
		long total = (endTime - startTime)/1000000;
		String prologDialect = newPrologEngine.prologDialect();
		StringBuilder sb = new StringBuilder();
		if(prologDialect == null) {
			logger.warn("Attempt to query the Prolog dialect property from the Prolog engine failed.");
			sb.append("An unknown");
		} else {
			sb.append("A " + prologDialect);
		}
		sb.append(" Prolog engine has been initialized in " + total + " milliseconds.");
		logger.info(sb.toString());
		
		return newPrologEngine;
	}

	public void onCreate(PrologEngine prologEngine) {
		//nothing by default, to be overridden if needed
	}

	/**
	 * 
	 */
	public void readyOrThrow() {
		//empty by default
	}
	
	/**
	 * Notify the listeners that the state of this driver is now disabled
	 * 
	 */
	protected void notifyDisabledState() {
		for(DriverStateListener listener : getListeners()) {
			listener.onDriverDisabled();
		}
	}
	
	public void addStateListener(DriverStateListener listener) {
		Collection<DriverStateListener> listeners = getListeners(); //it is important to use the getter here instead of direct access since the listeners may be managed in a different way by descendant classes
		if(!listeners.contains(listener))
			listeners.add(listener); 
	}
	
	public void removeStateListener(DriverStateListener listener) {
		getListeners().add(listener); //it is important to use the getter here instead of direct access since the listeners may be managed in a different way by descendant classes
	}
	
	/**
	 * Subclasses may override this method to provide a convenient representation of the state listeners.
	 * Concretely, drivers using libraries that can be instantiated only once, may implement this method as returning a static shared variable
	 * (care should be taken since this may pose a problem if the driver class is loaded by multiple classloaders).
	 * @return the state listeners of this driver
	 */
	protected Collection<DriverStateListener> getListeners() {
		return listeners;
	}
	
	@Override
	public String toString() {
		return getDescription();
	}
	
	public String getShortDescription() {
		return getEngineName() + "-" + getLibraryName();
	}
	
	public String getDescription() {
		return "This driver connects a " + getEngineName() + " Prolog engine by means of the " + getLibraryName() +" library.";
	}
	
	/**
	 * 
	 * @return a instance of a Prolog engine. This instance has not been configured yet
	 */
	protected abstract T basicCreatePrologEngine();
	
	public abstract String getLibraryName();
	
	public abstract String getEngineName();

	public String getLicenseUrl() {
		return "";
	}
	
	public String getSiteUrl() {
		return "";
	}
	
}
