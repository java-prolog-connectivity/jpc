package org.jpc.converter.catalog.error;

import static org.jpc.converter.catalog.error.IsoPrologErrorConverter.isIsoPrologError;

import java.lang.reflect.Type;

import org.jpc.Jpc;
import org.jpc.converter.JpcConversionException;
import org.jpc.converter.JpcConverter;
import org.jpc.error.DomainError;
import org.jpc.error.EvaluationError;
import org.jpc.term.Compound;
import org.jpc.term.Term;

public class EvaluationErrorConverter extends JpcConverter<EvaluationError, Compound> {

	public static final String EVALUATION_ERROR_FUNCTOR = "evaluation_error";
	
	public static boolean isEvaluationError(Term term) {
		return isIsoPrologError(term) && term.arg(1).hasFunctor(EVALUATION_ERROR_FUNCTOR, 1);
	}
	
	@Override
	public EvaluationError fromTerm(Compound term, Type type, Jpc context) {
		if(!isEvaluationError(term) || !type.equals(EvaluationError.class))
			throw new JpcConversionException();
		return new EvaluationError(term);
	}
	
}
