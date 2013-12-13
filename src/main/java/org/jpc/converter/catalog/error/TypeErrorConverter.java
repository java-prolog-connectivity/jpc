package org.jpc.converter.catalog.error;

import static org.jpc.converter.catalog.error.IsoPrologErrorConverter.isIsoPrologError;

import java.lang.reflect.Type;

import org.jconverter.converter.ConversionException;
import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.error.TypeError;
import org.jpc.term.Compound;
import org.jpc.term.Term;

public class TypeErrorConverter implements FromTermConverter<Compound, TypeError> {

	public static final String TYPE_ERROR_FUNCTOR = "type_error";
	
	public static boolean isTypeError(Term term) {
		return isIsoPrologError(term) && term.arg(1).hasFunctor(TYPE_ERROR_FUNCTOR, 2);
	}
	
	@Override
	public TypeError fromTerm(Compound term, Type type, Jpc context) {
		if(!isTypeError(term))
			throw new ConversionException();
		return new TypeError(term);
	}
	
}
