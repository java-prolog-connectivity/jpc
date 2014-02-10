package org.jpc.term.jterm;

import java.lang.ref.ReferenceQueue;
import java.lang.ref.WeakReference;

import org.jpc.converter.TermConvertable;
import org.jpc.term.Compound;
import org.minitoolbox.reference.Cleanable;

/**
 * A weak reference associated with an arbitrary term identifier (a Compound term).
 * @author sergioc
 *
 * @param <REF_TYPE> the reference type
 */
public class JTermRef<REF_TYPE> extends WeakReference<REF_TYPE> implements Cleanable, TermConvertable<Compound> {
	
	private final Compound refId; //the term identifier for this reference.
	private final Runnable cleaningTask; //an arbitrary cleaning task to be executed when the referent is garbage collected.
	
	JTermRef(REF_TYPE referent, ReferenceQueue<REF_TYPE> referenceQueue, Compound refId) {
		this(referent, referenceQueue, refId, null);
	}
	
	JTermRef(REF_TYPE referent, ReferenceQueue<REF_TYPE> referenceQueue, Compound refId, Runnable cleaningTask) {
		super(referent, referenceQueue);
		this.refId = refId;
		this.cleaningTask = cleaningTask;
	}

	/**
	 * 
	 * @return the term representation of the referent.
	 */
	public Compound getRefId() {
		return refId;
	}

	/**
	 * Callback method that should be invoked when the referenced object has been garbage collected.
	 */
	@Override
	public void cleanUp() {
		if(cleaningTask != null)
			cleaningTask.run();
	}
	
	@Override
	public Compound asTerm() {
		return getRefId();
	}
	
	@Override
	public String toString() {
		return asTerm().toString();
	}

	@Override
	public int hashCode() {
		return refId.hashCode();
	}

	@Override
	public boolean equals(Object obj) {
		if(this == obj)
			return true;
		if(obj instanceof JTermRef) {
			return refId.equals(((JTermRef<?>)obj).getRefId()) && 
					(this.get() == ((JTermRef<?>)obj).get());
		}
		return false;
	}

}
