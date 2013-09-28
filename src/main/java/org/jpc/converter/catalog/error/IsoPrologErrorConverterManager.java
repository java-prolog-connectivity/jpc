package org.jpc.converter.catalog.error;

import java.lang.reflect.Type;

import org.jpc.converter.ConverterManager;
import org.jpc.error.IsoPrologError;
import org.jpc.term.Compound;
import org.jpc.term.Term;

public class IsoPrologErrorConverterManager extends ConverterManager<IsoPrologError, Compound> {

	public IsoPrologErrorConverterManager() {
		registerIsoPrologErrorConverters();
	}
	
	@Override
	public boolean canConvertFromTerm(Term term, Type toType) {
		return super.canConvertFromTerm(term, toType) && IsoPrologErrorConverter.isIsoPrologError(term);
		
	}
	private void registerIsoPrologErrorConverters() {
		register(new IsoPrologErrorConverter());
		register(new DomainErrorConverter());
		register(new EvaluationErrorConverter());
		register(new ExistenceErrorConverter());
		register(new InstantiationErrorConverter());
		register(new PermissionErrorConverter());
		register(new RepresentationErrorConverter());
		register(new ResourceErrorConverter());
		register(new SyntaxErrorConverter());
		register(new SystemErrorConverter());
		register(new TypeErrorConverter());
	}

}
