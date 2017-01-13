package org.jpc.internal.gc;

import java.lang.ref.PhantomReference;
import java.lang.ref.ReferenceQueue;


public class CleanablePhantomReference<T> extends PhantomReference<T> implements Cleanable {
	
	private final Runnable cleaningTask; //a cleaning task to be executed when the referent is garbage collected.

	public CleanablePhantomReference(T referent, ReferenceQueue<? super T> q, Runnable cleaningTask) {
		super(referent, q);
		this.cleaningTask = cleaningTask;
	}

	/**
	 * Callback method that should be invoked when the referenced object has been garbage collected.
	 */
	@Override
	public void cleanUp() {
		if(cleaningTask != null)
			cleaningTask.run();
	}
}