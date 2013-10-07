package org.jpc.term.jterm;

import java.lang.ref.ReferenceQueue;


public class WeakReferencesCleaner extends Thread {

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
				JTerm<?> weakRef = (JTerm<?>) referenceQueue.remove();
				weakRef.cleanUp();
			} catch (InterruptedException e) {}
		}
	}

}
