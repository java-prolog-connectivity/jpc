package org.jpc.converter.catalog.primitive;

import static org.jpc.engine.prolog.PrologConstants.FAIL;
import static org.jpc.engine.prolog.PrologConstants.FALSE;
import static org.jpc.engine.prolog.PrologConstants.TRUE;

import java.lang.reflect.Type;

import org.jconverter.converter.ConversionException;
import org.jpc.Jpc;
import org.jpc.converter.BidirectionalTermConverter;
import org.jpc.term.Atom;

public class BooleanConverter implements BidirectionalTermConverter<Boolean, Atom> {

	@Override
	public Atom toTerm(Boolean bool, Class<Atom> termClass, Jpc context) {
		Atom term;
		if(bool)
			term = Atom.TRUE_TERM;
		else
			term = Atom.FAIL_TERM;
		return term;
	}
	
	@Override
	public Boolean fromTerm(Atom atom, Type type, Jpc context) {
		if(!Boolean.class.equals(type))
			throw new ConversionException();
		if(atom.getName().equals(TRUE))
			return true;
		else if(atom.getName().equals(FAIL) || atom.getName().equals(FALSE))
			return false;
		else
			throw new ConversionException();
	}
}
