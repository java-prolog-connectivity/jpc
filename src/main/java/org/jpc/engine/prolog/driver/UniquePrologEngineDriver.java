package org.jpc.engine.prolog.driver;

import java.util.Collection;

import org.jpc.engine.listener.DriverStateListener;
import org.jpc.engine.prolog.PrologEngine;
import org.jpc.util.JpcPreferences;
import org.jpc.util.engine.supported.EngineDescription;

/**
 * This class describes logic engines that are unique per accept
 * Instances can cache the logic engine when creating it
 * @author sergioc
 *
 */
public abstract class UniquePrologEngineDriver<T extends PrologEngine> extends AbstractPrologEngineDriver<T> {
	
	public UniquePrologEngineDriver(EngineDescription engineDescription) {
		super(engineDescription);
	}
	
	public UniquePrologEngineDriver(EngineDescription engineDescription, JpcPreferences preferences) {
		super(engineDescription, preferences);
	}
	
	@Override
	public synchronized boolean isDisabled() {
		return isInstanceRunning();
	}
	
	protected abstract boolean isInstanceRunning();
	
	@Override
	protected synchronized T createPrologEngine(PrologEngineFactory<T> basicFactory) {
		if(isInstanceRunning()) {
			throw new UnsupportedOperationException("No more than one Prolog Engine can be created by this configuration");
		}
		else {
			notifyDisabledState(); //notify that the driver cannot create more Prolog engines than the one to be created
			T prologEngine = super.createPrologEngine(basicFactory);
			return prologEngine;
		}
	}
	
	/**
	 * To force descendant to re-implement this method properly.
	 * Since descendants represent drivers that can create only one session per JVM, descendant drivers should refer to the same collection of listeners.
	 */
	protected abstract Collection<DriverStateListener> getListeners();

}
