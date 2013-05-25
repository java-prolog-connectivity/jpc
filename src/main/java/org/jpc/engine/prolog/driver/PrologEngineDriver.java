package org.jpc.engine.prolog.driver;

import org.jpc.engine.listener.DriverStateListener;
import org.jpc.engine.prolog.PrologEngine;

public interface PrologEngineDriver<T extends PrologEngine> extends PrologEngineFactory<T> {

	public void addStateListener(DriverStateListener listener);
	
	public void removeStateListener(DriverStateListener listener);
	
	public String getShortDescription();
	
	public String getDescription();
	
	public String getLibraryName();
	
	public String getEngineName();
	
	public String getLicenseUrl();
	
	public String getSiteUrl();

}
