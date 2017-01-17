package org.jpc.engine.prolog.driver;

import org.jpc.engine.listener.DriverStateListener;
import org.jpc.engine.prolog.PrologEngine;
import org.jpc.util.engine.supported.EngineDescription;

public interface PrologEngineDriver<T extends PrologEngine> extends PrologEngineFactory<T> {

	void addStateListener(DriverStateListener listener);
	
	void removeStateListener(DriverStateListener listener);
	
	String getShortDescription();
	
	String getDescription();
	
	String getLibraryName();
	
	EngineDescription getEngineDescription();
	
	String getLicenseUrl();
	
	String getSiteUrl();

	/**
	 * 
	 * @return true if the driver cannot create Prolog engines. false otherwise.
	 */
	boolean isDisabled();
	
}
