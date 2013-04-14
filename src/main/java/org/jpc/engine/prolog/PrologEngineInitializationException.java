package org.jpc.engine.prolog;

import org.jpc.JpcException;

/**
 * This exception is thrown if a PrologEngineConfiguration is missing some data to start a logic engine, or if the existing data is incorrect
 * @author sergioc
 *
 */
public class PrologEngineInitializationException extends JpcException {

	public PrologEngineInitializationException() {
	}
	
	public PrologEngineInitializationException(String message) {
		super(message);
	}
	
	public PrologEngineInitializationException(Exception e) {
		super(e);
	}
}
