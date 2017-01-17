package org.jpc.internal.concurrent;

import java.util.concurrent.Executor;


/**
 * A dummy syncrhronous executor useful for testing.
 * @author sergioc
 *
 */
public class SynchronousExecutor implements Executor {

	@Override
	public void execute(Runnable command) {
		command.run();
	}
	
}
