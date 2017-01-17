package org.jpc.internal.concurrent;

import java.util.concurrent.ThreadFactory;

/**
 * Allows the creation of one single thread, will fail in any subsequent calls instead of creating a new thread
 * This is useful for Executor classes that should not start more than one thread.
 * In this case, if the unique thread dies an exception should be thrown when the class receives a request to create a new thread.
 * @author sergioc
 *
 */
public class OneThreadFactory implements ThreadFactory {
	
	private boolean oneThreadCreated;
	
	@Override
	public synchronized Thread newThread(Runnable r) {
		if(!oneThreadCreated) {
			Thread thread = new Thread(r);
			oneThreadCreated = true;
			return thread;
		} else
			throw new RuntimeException("Thread factory cannot create more than one Thread");
	}

}
