package org.jpc;

import java.lang.reflect.Type;

import org.jpc.converter.ConverterManager;
import org.jpc.converter.DefaultJpcConverterManager;
import org.jpc.converter.instantiation.DefaultInstantiationManager;
import org.jpc.converter.instantiation.InstantiationManager;
import org.jpc.converter.typesolver.DefaultTypeSolverManager;
import org.jpc.converter.typesolver.TypeSolverManager;
import org.jpc.error.handling.DefaultJpcErrorHandler;
import org.jpc.error.handling.ErrorHandler;
import org.jpc.term.Term;

public class DefaultJpc extends Jpc {

	private ConverterManager converterManager;
	private TypeSolverManager typeSolverManager;
	private InstantiationManager instantiationManager;
	private ErrorHandler errorHandler;
	//private JpcPreferences preferences;

	public DefaultJpc() {
		this.converterManager = new DefaultJpcConverterManager();
		this.typeSolverManager = new DefaultTypeSolverManager();
		this.instantiationManager = new DefaultInstantiationManager();
		this.errorHandler = new DefaultJpcErrorHandler();
	}
	
	public DefaultJpc(ConverterManager converterManager, TypeSolverManager typeSolverManager, InstantiationManager instantiationManager, ErrorHandler errorHandler) {
		this.typeSolverManager = typeSolverManager;
		this.converterManager = converterManager;
		this.instantiationManager = instantiationManager;
		this.errorHandler = errorHandler;
		//this.preferences = preferences;
	}
	
	@Override
	public <T> T fromTerm(Term term, Type type) {
		return (T) converterManager.fromTerm(term, type, this);
	}
	
	@Override
	public <T extends Term> T toTerm(Object object, Class<T> termClass) {
		return converterManager.toTerm(object, termClass, this);
	}
	
	@Override
	public <T> T instantiate(Type targetType) {
		return instantiationManager.instantiate(targetType);
	}

	@Override
	public Type getType(Term term) {
		return typeSolverManager.getType(term);
	}

	@Override
	public boolean handleError(Term errorTerm, Term goal) {
		return errorHandler.handle(errorTerm, goal, this);
	}

}
