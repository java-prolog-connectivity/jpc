package org.jpc.term.jterm;

import java.lang.ref.ReferenceQueue;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


/**
 * Internal class. Implemented to execute cleaning tasks of JTerm references at the moment they are garbage collected.
 * @author sergioc
 *
 */
public class WeakReferencesCleaner extends Thread {

	private static Logger logger = LoggerFactory.getLogger(WeakReferencesCleaner.class);
	
	private static WeakReferencesCleaner referencesCleaner = new WeakReferencesCleaner(new ReferenceQueue<Object>());
	
	public static WeakReferencesCleaner getWeakReferencesCleaner() {
		return referencesCleaner;
	}
	
	public synchronized static void startWeakReferencesCleaner() {
		if(!referencesCleaner.isAlive())
			referencesCleaner.start();
	}
	
	private ReferenceQueue<?> referenceQueue;
	
	public WeakReferencesCleaner(ReferenceQueue<?> referenceQueue) {
		this(referenceQueue, Thread.MAX_PRIORITY);
	}
	
	public WeakReferencesCleaner(ReferenceQueue<?> referenceQueue, int priority) {
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
				JTermRef<?> jTermRef = (JTermRef<?>) referenceQueue.remove();
				try {
					jTermRef.cleanUp();
				} catch(Exception e) {
					logger.error("Exception thrown while executing jterm cleaning task: " + e.toString());
				}
			} catch (InterruptedException e) {}
		}
	}

}
