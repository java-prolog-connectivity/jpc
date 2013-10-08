package org.jpc.term.jterm;

import java.lang.ref.ReferenceQueue;
import java.lang.ref.WeakReference;

import org.jpc.converter.TermConvertable;
import org.jpc.term.Compound;

/**
 * A weak reference associated with an arbitrary term identifier (a Compound term).
 * @author sergioc
 *
 * @param <REF_TYPE> the reference type
 */
public class JTerm<REF_TYPE> extends WeakReference<REF_TYPE> implements TermConvertable<Compound> {
	
	private Compound refId; //the term identifier for this reference.
	private JTermManager jTermManager; //the term manager of this reference.
	
	public JTerm(REF_TYPE referent, ReferenceQueue<REF_TYPE> referenceQueue, Compound refId, JTermManager jTermManager) {
		super(referent, referenceQueue);
		this.refId = refId;
		this.jTermManager = jTermManager;
	}

	public Compound getRefId() {
		return refId;
	}

	/**
	 * Callback method that should be invoked when the referenced object has been garbage collected.
	 */
	void cleanUp() {
		jTermManager.remove(getRefId());
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
		if(obj instanceof JTerm) {
			return refId.equals(((JTerm<?>)obj).getRefId()) && 
					(this.get() == ((JTerm<?>)obj).get());
		}
		return false;
	}

}
