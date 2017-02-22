package org.jpc.converter.catalog.error;

import static org.jconverter.converter.ConversionGoal.conversionGoal;
import static org.jpc.converter.catalog.error.IsoPrologErrorConverter.isIsoPrologError;

import org.jconverter.converter.DelegateConversionException;
import org.jconverter.converter.TypeDomain;
import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.error.SyntaxError;
import org.jpc.term.Compound;
import org.jpc.term.Term;

public class SyntaxErrorConverter implements FromTermConverter<Compound, SyntaxError> {

	public static final String SYNTAX_ERROR_FUNCTOR_NAME = "syntax_error";
	
	public static boolean isSyntaxError(Term term) {
		return isIsoPrologError(term) && term.arg(1).hasFunctor(SYNTAX_ERROR_FUNCTOR_NAME, 1);
	}
	
	@Override
	public SyntaxError fromTerm(Compound term, TypeDomain target, Jpc context) {
		if (!isSyntaxError(term)) {
			throw new DelegateConversionException(conversionGoal(term, target));
		}
		return new SyntaxError(term);
	}
	
}
