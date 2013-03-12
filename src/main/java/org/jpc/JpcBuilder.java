package org.jpc;

import org.jpc.converter.ConverterManager;
import org.jpc.converter.DefaultJpcConverterManager;
import org.jpc.converter.JpcConverter;
import org.jpc.error.handling.DefaultJpcErrorHandler;
import org.jpc.error.handling.ErrorHandler;
import org.jpc.error.handling.ErrorHandlerManager;
import org.jpc.instantiationmanager.DefaultInstantiationManager;
import org.jpc.instantiationmanager.InstantiationManager;
import org.jpc.typesolver.DefaultTypeSolverManager;
import org.jpc.typesolver.TermTypeSolver;
import org.jpc.typesolver.TypeSolverManager;
import org.minitoolbox.pattern.Factory;

public class JpcBuilder {

	private ConverterManager converterManager;
	private TypeSolverManager typeSolverManager;
	private InstantiationManager instantiationManager;
	private ErrorHandlerManager errorHandlerManager;
	
	public JpcBuilder() {
		this.converterManager = new DefaultJpcConverterManager();
		this.typeSolverManager = new DefaultTypeSolverManager();
		this.instantiationManager = new DefaultInstantiationManager();
		this.errorHandlerManager = new DefaultJpcErrorHandler();
	}
	
	public Jpc create() {
		return new Jpc(converterManager, typeSolverManager, instantiationManager, errorHandlerManager);
	}

	public JpcBuilder registerConverter(JpcConverter<?,?> converter) {
		converterManager.register(converter);
		return this;
	}
	
	public JpcBuilder registerTypeSolver(TermTypeSolver typeSolver) {
		typeSolverManager.register(typeSolver);
		return this;
	}
	
	public <CT> JpcBuilder registerInstantiationManager(Class<CT> clazz, Factory<CT> factory) {
		instantiationManager.register(clazz, factory);
		return this;
	}
	
	public void registerErrorHandler(ErrorHandler errorHandler) {
		errorHandlerManager.register(errorHandler);
	}
	
//	//TODO
//	public void setPreferences(JpcPreferences preferences) {
//		throw new NotYetImplementedException();
//	}

}
