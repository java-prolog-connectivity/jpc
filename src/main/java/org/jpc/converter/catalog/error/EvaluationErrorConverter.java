package org.jpc.converter.catalog.error;

import static org.jconverter.converter.ConversionGoal.conversionGoal;
import static org.jpc.converter.catalog.error.IsoPrologErrorConverter.isIsoPrologError;

import org.jconverter.converter.DelegateConversionException;
import org.jconverter.converter.TypeDomain;
import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.error.EvaluationError;
import org.jpc.term.Compound;
import org.jpc.term.Term;

public class EvaluationErrorConverter implements FromTermConverter<Compound, EvaluationError> {

	public static final String EVALUATION_ERROR_FUNCTOR_NAME = "evaluation_error";
	
	public static boolean isEvaluationError(Term term) {
		return isIsoPrologError(term) && term.arg(1).hasFunctor(EVALUATION_ERROR_FUNCTOR_NAME, 1);
	}
	
	@Override
	public EvaluationError fromTerm(Compound term, TypeDomain target, Jpc context) {
		if (!isEvaluationError(term)) {
			throw new DelegateConversionException(conversionGoal(term, target));
		}
		return new EvaluationError(term);
	}
	
}
