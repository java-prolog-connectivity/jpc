package org.jpc.engine.prolog.configuration;

import org.jpc.JpcException;

/**
 * This exception is thrown if a PrologEngineConfiguration is missing some data to start a logic engine, or if the existing data is incorrect
 * @author sergioc
 *
 */
public class EngineConfigurationException extends JpcException {

	public EngineConfigurationException() {
	}
	
	public EngineConfigurationException(String message) {
		super(message);
	}
	
	public EngineConfigurationException(Exception e) {
		super(e);
	}
}
