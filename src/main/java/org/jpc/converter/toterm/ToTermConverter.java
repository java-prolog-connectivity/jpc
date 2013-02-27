package org.jpc.converter.toterm;

import org.jpc.Jpc;
import org.jpc.converter.JpcConverter;
import org.jpc.term.Term;

public abstract class ToTermConverter<S> extends JpcConverter<S, Term> {

	public abstract Term convert(S source, Jpc context);

	public boolean canConvert(Object object) {
		return sourceTypeIsAssignableTo(object.getClass());
	}
	
}
