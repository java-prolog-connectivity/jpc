package org.jpc.converter.catalog.error;

import static org.jpc.converter.catalog.error.IsoPrologErrorConverter.isIsoPrologError;

import java.lang.reflect.Type;

import org.jconverter.converter.ConversionException;
import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.error.RepresentationError;
import org.jpc.term.Compound;
import org.jpc.term.Term;

public class RepresentationErrorConverter implements FromTermConverter<Compound, RepresentationError> {

	public static final String REPRESENTATION_ERROR_FUNCTOR = "representation_error";
	
	public static boolean isRepresentationError(Term term) {
		return isIsoPrologError(term) && term.arg(1).hasFunctor(REPRESENTATION_ERROR_FUNCTOR, 1);
	}
	
	@Override
	public RepresentationError fromTerm(Compound term, Type type, Jpc context) {
		if(!isRepresentationError(term))
			throw new ConversionException();
		return new RepresentationError(term);
	}
	
}
