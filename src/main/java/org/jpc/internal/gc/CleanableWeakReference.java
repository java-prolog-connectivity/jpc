package org.jpc.internal.gc;

import java.lang.ref.ReferenceQueue;
import java.lang.ref.WeakReference;

public class CleanableWeakReference<T> extends WeakReference<T> implements Cleanable {
	
	private final Runnable cleaningTask; //a cleaning task to be executed when the referent is garbage collected.
	
	public CleanableWeakReference(T referent, Runnable cleaningTask) {
		super(referent);
		this.cleaningTask = cleaningTask;
	}
	
	public CleanableWeakReference(T referent, ReferenceQueue<? super T> q, Runnable cleaningTask) {
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