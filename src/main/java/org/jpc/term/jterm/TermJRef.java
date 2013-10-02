package org.jpc.term.jterm;

import java.lang.ref.ReferenceQueue;

import org.jpc.converter.TermConvertable;
import org.jpc.term.Term;

public class TermJRef<T> extends IdentifiableWeakReference<Term, T> implements TermConvertable<Term> {

	public TermJRef(IdentifiableWeakReferenceManager<Term, T> weakReferenceManager, Term idTerm, T referent, ReferenceQueue<T> referenceQueue) {
		super(weakReferenceManager, idTerm, referent, referenceQueue);
	}

	@Override
	public Term asTerm() {
		return getRefId();
	}
	
	@Override
	public String toString() {
		return asTerm().toString();
	}

}
