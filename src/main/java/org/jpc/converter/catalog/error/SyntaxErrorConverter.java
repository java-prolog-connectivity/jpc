package org.jpc.converter.catalog.error;

import static org.jpc.converter.catalog.error.IsoPrologErrorConverter.isIsoPrologError;

import java.lang.reflect.Type;

import org.jconverter.converter.ConversionException;
import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.error.SyntaxError;
import org.jpc.term.Compound;
import org.jpc.term.Term;

public class SyntaxErrorConverter implements FromTermConverter<Compound, SyntaxError> {

	public static final String SYNTAX_ERROR_FUNCTOR = "syntax_error";
	
	public static boolean isSyntaxError(Term term) {
		return isIsoPrologError(term) && term.arg(1).hasFunctor(SYNTAX_ERROR_FUNCTOR, 1);
	}
	
	@Override
	public SyntaxError fromTerm(Compound term, Type type, Jpc context) {
		if(!isSyntaxError(term) || !type.equals(SyntaxError.class))
			throw new ConversionException();
		return new SyntaxError(term);
	}
	
}
