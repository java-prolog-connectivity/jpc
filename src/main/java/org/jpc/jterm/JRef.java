package org.jpc.jterm;

import static java.util.Arrays.asList;

import java.lang.ref.ReferenceQueue;
import java.lang.ref.WeakReference;

import org.jpc.converter.TermConvertable;
import org.jpc.term.Compound;
import org.jpc.term.IntegerTerm;

public class JRef extends WeakReference<Object> implements TermConvertable<Compound> {

	public static final String JREF_FUNCTOR = "jref";
	
	private RefId refId;
	
	JRef(Object referent, ReferenceQueue<Object> referenceQueue) {
		super(referent, referenceQueue);
		refId = RefIdManager.getDefaultRefIdManager().getOrCreate(referent);
	}

	public RefId getRefId() {
		return refId;
	}

	@Override
	public String toString() {
		return asTerm().toString();
	}
	
	@Override
	public Compound asTerm() {
		return new Compound(JREF_FUNCTOR, asList(new IntegerTerm(refId.getId())));
	}

}
