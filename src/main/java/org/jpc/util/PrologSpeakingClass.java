package org.jpc.util;

import java.util.List;

import org.jpc.Jpc;
import org.jpc.term.Term;
import org.jpc.util.reification.ReflectiveClass;

public class PrologSpeakingClass<T> extends PrologSpeakingObject {

	public PrologSpeakingClass(Class<T> wrappedClass, Jpc jpc) {
		super(new ReflectiveClass(wrappedClass), jpc);
	}

	public Class<T> getWrappedClass() {
		return (Class<T>) getWrappedObject();
	}
	
	public T newInstance(List<? extends Term> argTerms) {
		return invoke("new", argTerms);
	}

}
