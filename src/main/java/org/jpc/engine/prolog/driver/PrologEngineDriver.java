package org.jpc.engine.prolog.driver;

import org.jpc.engine.listener.DriverStateListener;
import org.jpc.engine.prolog.PrologEngine;
import org.jpc.util.engine.supported.EngineDescription;

public interface PrologEngineDriver<T extends PrologEngine> extends PrologEngineFactory<T> {

	public void addStateListener(DriverStateListener listener);
	
	public void removeStateListener(DriverStateListener listener);
	
	public String getShortDescription();
	
	public String getDescription();
	
	public String getLibraryName();
	
	public EngineDescription getEngineDescription();
	
	public String getLicenseUrl();
	
	public String getSiteUrl();

	/**
	 * 
	 * @return true if the driver cannot create Prolog engines. false otherwise.
	 */
	public boolean isDisabled();
	
}
