package org.jpc.converter.toterm;

import org.jpc.Jpc;
import org.jpc.term.Atom;
import org.jpc.term.Term;

public class BooleanToTermConverter extends ToTermConverter<Boolean> {

	@Override
	public Term convert(Boolean bool, Jpc context) {
		if(bool)
			return Atom.TRUE_TERM;
		else
			return Atom.FALSE_TERM;
	}

}
