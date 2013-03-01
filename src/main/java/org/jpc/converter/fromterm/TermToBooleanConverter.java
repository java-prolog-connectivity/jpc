package org.jpc.converter.fromterm;

import static org.jpc.engine.prolog.PrologConstants.FAIL;
import static org.jpc.engine.prolog.PrologConstants.FALSE;
import static org.jpc.engine.prolog.PrologConstants.TRUE;

import java.lang.reflect.Type;

import org.jpc.Jpc;
import org.jpc.converter.JpcConversionException;
import org.jpc.term.Atom;
import org.jpc.term.Term;

public class TermToBooleanConverter extends FromTermConverter<Boolean> {

	@Override
	public Boolean convert(Term term, Type type, Jpc context) {
		if(!Boolean.class.equals(type) || !(term instanceof Atom))
			throw new JpcConversionException();
		Atom atom = (Atom) term;
		if(atom.getName().equals(TRUE))
			return true;
		else if(atom.getName().equals(FAIL) || atom.getName().equals(FALSE))
			return false;
		else
			throw new JpcConversionException();
	}

}
