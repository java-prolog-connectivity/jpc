package org.jpc.engine.provider;

import org.jpc.engine.prolog.PrologEngine;

/**
 * Currently used for unit testing.
 * @author sergioc
 *
 */
public class SimpleEngineProvider<T extends PrologEngine> implements PrologEngineProvider<T> {

	private T prologEngine;
	
	public SimpleEngineProvider(T prologEngine) {
		this.prologEngine = prologEngine;
	}
	
	@Override
	public T getPrologEngine() {
		return prologEngine;
	}
	
}
