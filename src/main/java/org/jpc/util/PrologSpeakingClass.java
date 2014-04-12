package org.jpc.util;

import java.util.List;

import org.jpc.Jpc;
import org.jpc.term.Term;

public class PrologSpeakingClass<T> extends PrologSpeakingObject {

	public PrologSpeakingClass(Class<T> wrappedClass, Jpc jpc) {
		super(wrappedClass, jpc);
	}

	public Class<T> getWrappedClass() {
		return (Class<T>) getWrappedObject();
	}
	
	@Override
	protected Object getTargetObject() {
		return null;
	}
	
	@Override
	protected Class<T> getTargetClass() {
		return getWrappedClass();
	}
	
	public T newInstance(List<Term> argTerms) {
		return invoke("new", argTerms);
	}

}
