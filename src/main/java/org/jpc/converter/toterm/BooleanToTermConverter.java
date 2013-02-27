package org.jpc.converter.toterm;

import org.jpc.Jpc;
import org.jpc.term.Atom;
import org.jpc.term.Term;

public class BooleanToTermConverter extends ToTermConverter<Boolean> {

	@Override
	public Term convert(Boolean source, Jpc context) {
		return new Atom(source.toString());
	}

}
