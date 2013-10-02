package org.jpc.term.jterm;

import static java.util.Arrays.asList;

import java.lang.ref.ReferenceQueue;

import org.jpc.converter.TermConvertable;
import org.jpc.term.Compound;
import org.jpc.term.IntegerTerm;

public class JRef<T> extends IdentifiableWeakReference<JRefId, T> implements TermConvertable<Compound> {

	public static final String JREF_FUNCTOR = "jref";

	JRef(IdentifiableWeakReferenceManager<JRefId, T> weakReferenceManager, JRefId jRefId, T referent, ReferenceQueue<T> referenceQueue) {
		super(weakReferenceManager, jRefId, referent, referenceQueue);
	}
	
	@Override
	public Compound asTerm() {
		return new Compound(JREF_FUNCTOR, asList(new IntegerTerm(getRefId().getId())));
	}

	@Override
	public String toString() {
		return asTerm().toString();
	}
	
}
