package org.jpc.internal.gc;

import java.lang.ref.Reference;
import java.lang.ref.ReferenceQueue;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


/**
 * Instance execute cleaning tasks of weak references at the moment they are garbage collected.
 * @author sergioc
 *
 */
public class ReferencesCleaner extends Thread {

	private static final Logger logger = LoggerFactory.getLogger(ReferencesCleaner.class);
	
	private static final ReferencesCleaner referencesCleaner = new ReferencesCleaner(new ReferenceQueue<Object>());
	
	public static ReferencesCleaner getDefault() {
		return referencesCleaner;
	}
	
	public synchronized static void startDefault() {
		if(!referencesCleaner.isAlive())
			referencesCleaner.start();
	}
	
	private final ReferenceQueue<?> referenceQueue;
	
	public ReferencesCleaner(ReferenceQueue<?> referenceQueue) {
		this(referenceQueue, Thread.MAX_PRIORITY);
	}
	
	public ReferencesCleaner(ReferenceQueue<?> referenceQueue, int priority) {
		this.referenceQueue = referenceQueue;
		this.setDaemon(true);
	}

	public ReferenceQueue<?> getReferenceQueue() {
		return referenceQueue;
	}
	
	@Override
	public void run() {
		while(true) {
			try {
				Reference<?> reference = referenceQueue.remove();
				if(reference instanceof Cleanable) {
					try {
						((Cleanable)reference).cleanUp();
					} catch(Exception e) {
						logger.error("Exception thrown while executing cleaning task: " + e.toString());
					}
				}
				
			} catch (InterruptedException e) {}
		}
	}

}
