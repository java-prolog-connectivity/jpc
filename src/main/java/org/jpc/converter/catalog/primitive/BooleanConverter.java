package org.jpc.converter.catalog.primitive;

import static org.jpc.term.Atom.FAIL;
import static org.jpc.term.Atom.FALSE;
import static org.jpc.term.Atom.TRUE;

import java.lang.reflect.Type;

import org.jconverter.converter.ConversionException;
import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.converter.ToTermConverter;
import org.jpc.term.Atom;

public class BooleanConverter implements ToTermConverter<Boolean, Atom>, FromTermConverter<Atom, Boolean> {

	@Override
	public Atom toTerm(Boolean bool, Class<Atom> termClass, Jpc context) {
		Atom term;
		if(bool)
			term = TRUE;
		else
			term = FALSE;
		return term;
	}
	
	@Override
	public Boolean fromTerm(Atom atom, Type type, Jpc context) {
		if(!Boolean.class.equals(type))
			throw new ConversionException();
		if(atom.equals(TRUE))
			return true;
		else if(atom.equals(FAIL) || atom.equals(FALSE))
			return false;
		else
			throw new ConversionException();
	}
}
