package org.jpc.converter.catalog.error;

import static org.jpc.converter.catalog.error.IsoPrologErrorConverter.isIsoPrologError;

import java.lang.reflect.Type;

import org.jpc.Jpc;
import org.jpc.converter.JpcConversionException;
import org.jpc.converter.JpcConverter;
import org.jpc.error.TypeError;
import org.jpc.term.Compound;
import org.jpc.term.Term;

public class TypeErrorConverter extends JpcConverter<TypeError, Compound> {

	public static final String TYPE_ERROR_FUNCTOR = "type_error";
	
	public static boolean isTypeError(Term term) {
		return isIsoPrologError(term) && term.arg(1).hasFunctor(TYPE_ERROR_FUNCTOR, 2);
	}
	
	@Override
	public TypeError fromTerm(Compound term, Type type, Jpc context) {
		if(!isTypeError(term) || !type.equals(TypeError.class))
			throw new JpcConversionException();
		return new TypeError(term);
	}
	
}
