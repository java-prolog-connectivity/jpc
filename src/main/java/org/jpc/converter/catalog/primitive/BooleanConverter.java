package org.jpc.converter.catalog.primitive;

import static org.jconverter.converter.ConversionGoal.conversionGoal;
import static org.jpc.term.Atom.FAIL;
import static org.jpc.term.Atom.FALSE;
import static org.jpc.term.Atom.TRUE;

import org.jconverter.converter.DelegateConversionException;
import org.jconverter.converter.TypeDomain;
import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.converter.ToTermConverter;
import org.jpc.term.Atom;

public class BooleanConverter implements ToTermConverter<Boolean, Atom>, FromTermConverter<Atom, Boolean> {

	@Override
	public Atom toTerm(Boolean bool, TypeDomain target, Jpc context) {
		Atom term;
		if(bool)
			term = TRUE;
		else
			term = FALSE;
		return term;
	}
	
	@Override
	public Boolean fromTerm(Atom atom, TypeDomain target, Jpc context) {
		if(!Boolean.class.equals(target.getType()))
			throw new DelegateConversionException(conversionGoal(atom, target));
		if(atom.equals(TRUE))
			return true;
		else if(atom.equals(FAIL) || atom.equals(FALSE))
			return false;
		else
			throw new DelegateConversionException(conversionGoal(atom, target));
	}
}
