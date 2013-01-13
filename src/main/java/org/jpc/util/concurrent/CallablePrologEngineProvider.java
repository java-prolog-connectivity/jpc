package org.jpc.util.concurrent;

import java.util.concurrent.Callable;

import org.jpc.engine.prolog.PrologEngine;
import org.jpc.engine.prolog.PrologEngineConfiguration;

/**
 * Wraps a Callable that requires a prolog engine
 * This class obtains a prolog engine from a ThreadLocal class. If the engine has not been created, it instantiates it according to the engine configuration sent in the constructor.
 * @author sergioc
 *
 * @param <T>
 */
public class CallablePrologEngineProvider<T> extends ThreadConnectionProvider implements Callable<T> {

	private JpcCallable<T> callable;
	
	public CallablePrologEngineProvider(PrologEngineConfiguration prologEngineConfiguration, JpcCallable<T> callable) {
		super(prologEngineConfiguration);
		this.callable = callable;
	}
	
	@Override
	public T call() throws Exception {
		PrologEngine prologEngine = getPrologEngine();
		callable.setPrologEngine(prologEngine);
		return callable.call();
	}

}
