package org.jpc.converter;

import org.jpc.Jpc;
import org.jpc.term.Term;

public interface ToTermConverter<U,T extends Term> extends JpcConverter {

	public T toTerm(U object, Class<T> termClass, Jpc context);
	
}
