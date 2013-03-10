package org.jpc.converter.catalog;

import static org.jpc.engine.prolog.PrologConstants.FAIL;
import static org.jpc.engine.prolog.PrologConstants.FALSE;
import static org.jpc.engine.prolog.PrologConstants.TRUE;

import java.lang.reflect.Type;

import org.jpc.Jpc;
import org.jpc.converter.JpcConversionException;
import org.jpc.converter.JpcConverter;
import org.jpc.term.Atom;
import org.jpc.term.Term;

public class BooleanConverter extends JpcConverter<Boolean, Atom> {

	@Override
	public <T extends Atom> T toTerm(Boolean bool, Class<T> termClass, Jpc context) {
		Term term;
		if(bool)
			term = Atom.TRUE_TERM;
		else
			term = Atom.FALSE_TERM;
		return (T) term;
	}
	
	@Override
	public Boolean fromTerm(Atom atom, Type type, Jpc context) {
		if(!Boolean.class.equals(type))
			throw new JpcConversionException();
		if(atom.getName().equals(TRUE))
			return true;
		else if(atom.getName().equals(FAIL) || atom.getName().equals(FALSE))
			return false;
		else
			throw new JpcConversionException();
	}
}
