package org.jpc.internal.gc;

import java.lang.ref.ReferenceQueue;
import java.lang.ref.SoftReference;

public class CleanableSoftReference<T> extends SoftReference<T> implements Cleanable {
	
	private final Runnable cleaningTask; //a cleaning task to be executed when the referent is garbage collected.
	
	public CleanableSoftReference(T referent, Runnable cleaningTask) {
		super(referent);
		this.cleaningTask = cleaningTask;
	}
	
	public CleanableSoftReference(T referent, ReferenceQueue<? super T> q, Runnable cleaningTask) {
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