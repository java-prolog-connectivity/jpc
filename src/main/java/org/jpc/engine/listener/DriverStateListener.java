package org.jpc.engine.listener;

/**
 * Currently used to register listeners interested in knowing when a UniquePrologEngineDriver driver instance cannot create more prolog sessions.
 * @author sergioc
 *
 */
public interface DriverStateListener {

	void onDriverDisabled();
	
}
