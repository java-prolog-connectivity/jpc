package org.jpc.engine.prolog.driver;

import java.util.Collection;

import org.jpc.JpcPreferences;
import org.jpc.engine.listener.DriverStateListener;
import org.jpc.engine.prolog.PrologEngine;

/**
 * This class describes logic engines that are unique per process
 * Instances can cache the logic engine when creating it
 * @author sergioc
 *
 */
public abstract class UniquePrologEngineDriver extends PrologEngineDriver {
	
	public UniquePrologEngineDriver() {
		super();
	}
	
	public UniquePrologEngineDriver(JpcPreferences preferences) {
		super(preferences);
	}
	
	@Override
	public boolean isEnabled() {
		return !isInstanceRunning();
	}
	
	public abstract boolean isInstanceRunning();
	
	@Override
	public synchronized PrologEngine createPrologEngine() {
		if(isInstanceRunning()) {
			throw new UnsupportedOperationException("No more than one Prolog Engine can be created by this configuration");
			//return basicCreatePrologEngine();
		}
		else {
			PrologEngine prologEngine = super.createPrologEngine();
			notifyDisabledState();
			return prologEngine;
		}
	}
	
	/**
	 * To force descendant to re-implement this method properly.
	 * Since descendants represent drivers that can create only one session per JVM, descendant drivers should refer to the same collection of listeners.
	 */
	protected abstract Collection<DriverStateListener> getListeners();

}
