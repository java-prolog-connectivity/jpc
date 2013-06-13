package org.jpc.engine.prolog.driver;

import org.jpc.engine.listener.DriverStateListener;
import org.jpc.engine.prolog.PrologEngine;
import org.jpc.util.supportedengines.EngineDescription;

public class PrologEngineDriverProxy<T extends PrologEngine> implements PrologEngineDriver<T> {

	private PrologEngineDriver<T> proxiedDriver;
	
	public PrologEngineDriverProxy(PrologEngineDriver<T> proxiedDriver) {
		this.proxiedDriver = proxiedDriver;
	}
	
	/**
	 * To allows descendants to instantiate the proxied driver in a custom way
	 */
	protected PrologEngineDriverProxy() {
	}

	protected void setProxiedDriver(PrologEngineDriver<T> proxiedDriver) {
		this.proxiedDriver = proxiedDriver;
	}
	
	public PrologEngineDriver<T> getProxiedDriver() {
		return proxiedDriver;
	}

	public T createPrologEngine() {
		return proxiedDriver.createPrologEngine();
	}

	public boolean isDisabled() {
		return proxiedDriver.isDisabled();
	}

	public void addStateListener(DriverStateListener listener) {
		proxiedDriver.addStateListener(listener);
	}

	public void removeStateListener(DriverStateListener listener) {
		proxiedDriver.removeStateListener(listener);
	}

	public String getShortDescription() {
		return proxiedDriver.getShortDescription();
	}

	public String getDescription() {
		return proxiedDriver.getDescription();
	}

	public String getLibraryName() {
		return proxiedDriver.getLibraryName();
	}

	public EngineDescription getEngineDescription() {
		return proxiedDriver.getEngineDescription();
	}

	public String getLicenseUrl() {
		return proxiedDriver.getLicenseUrl();
	}

	public String getSiteUrl() {
		return proxiedDriver.getSiteUrl();
	}
	
}
