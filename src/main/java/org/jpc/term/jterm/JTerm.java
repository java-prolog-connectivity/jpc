package org.jpc.term.jterm;

import java.lang.ref.ReferenceQueue;
import java.lang.ref.WeakReference;

import org.jpc.converter.TermConvertable;
import org.jpc.term.Compound;

/**
 * A weak reference with a term id.
 * @author sergioc
 *
 * @param <REF_TYPE> the reference class
 */
public class JTerm<REF_TYPE> extends WeakReference<REF_TYPE> implements TermConvertable<Compound> {
	
	private Compound refId;
	private JTermManager jTermManager;
	
	public JTerm(REF_TYPE referent, ReferenceQueue<REF_TYPE> referenceQueue, Compound refId, JTermManager jTermManager) {
		super(referent, referenceQueue);
		this.refId = refId;
		this.jTermManager = jTermManager;
	}

	public Compound getRefId() {
		return refId;
	}

	public void cleanUp() {
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
		if(obj instanceof JTerm) {
			return refId.equals(((JTerm<?>)obj).getRefId()) && 
					(this.get() == ((JTerm<?>)obj).get());
		}
		return false;
	}

}
