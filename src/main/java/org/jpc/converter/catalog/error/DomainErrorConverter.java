package org.jpc.converter.catalog.error;

import java.lang.reflect.Type;
import static org.jpc.converter.catalog.error.IsoPrologErrorConverter.isIsoPrologError;
import org.jpc.Jpc;
import org.jpc.converter.JpcConversionException;
import org.jpc.converter.JpcConverter;
import org.jpc.error.DomainError;
import org.jpc.term.Compound;
import org.jpc.term.Term;

public class DomainErrorConverter extends JpcConverter<DomainError, Compound> {

	public static final String DOMAIN_ERROR_FUNCTOR = "domain_error";
	
	public static boolean isDomainError(Term term) {
		return isIsoPrologError(term) && term.arg(1).hasFunctor(DOMAIN_ERROR_FUNCTOR, 2);
	}
	
	@Override
	public DomainError fromTerm(Compound term, Type type, Jpc context) {
		if(!isDomainError(term) || !type.equals(DomainError.class))
			throw new JpcConversionException();
		return new DomainError(term);
	}

}
