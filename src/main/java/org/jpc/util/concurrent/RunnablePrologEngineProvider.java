package org.jpc.util.concurrent;

import org.jpc.engine.prolog.PrologEngine;
import org.jpc.engine.prolog.PrologEngineConfiguration;

/**
 * Wraps a Runnable that requires a prolog engine
 * This class obtains a prolog engine from a ThreadLocal class. If the engine has not been created, it instantiates it according to the engine configuration sent in the constructor.
 * @author sergioc
 *
 */
public class RunnablePrologEngineProvider extends ThreadConnectionProvider implements Runnable {

	private JpcRunnable runnable;
	
	public RunnablePrologEngineProvider(PrologEngineConfiguration prologEngineConfiguration, JpcRunnable runnable) {
		super(prologEngineConfiguration);
		this.runnable = runnable;
	}

	@Override
	public void run() {
		PrologEngine prologEngine = getPrologEngine();
		runnable.setPrologEngine(prologEngine);
		runnable.run();
	}

}
