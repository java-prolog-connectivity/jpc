package org.jpc;

import org.jpc.converter.ConverterManager;
import org.jpc.converter.DefaultJpcConverterManager;
import org.jpc.converter.JpcConverter;
import org.jpc.converter.instantiation.DefaultInstantiationManager;
import org.jpc.converter.instantiation.InstanceCreator;
import org.jpc.converter.instantiation.InstantiationManager;
import org.jpc.converter.typesolver.DefaultTypeSolverManager;
import org.jpc.converter.typesolver.TermTypeSolver;
import org.jpc.converter.typesolver.TypeSolverManager;
import org.jpc.error.handling.DefaultJpcErrorHandler;
import org.jpc.error.handling.ErrorHandler;
import org.jpc.error.handling.ErrorHandlerManager;
import org.jpc.term.jterm.RefManager;

public class JpcBuilder {

	private ConverterManager converterManager;
	private TypeSolverManager typeSolverManager;
	private InstantiationManager instantiationManager;
	private ErrorHandlerManager errorHandlerManager;
	private RefManager refManager;
	
	public static JpcBuilder create() {
		return new JpcBuilder();
	}
	
	private JpcBuilder() {
		this.converterManager = new DefaultJpcConverterManager();
		this.typeSolverManager = new DefaultTypeSolverManager();
		this.instantiationManager = new DefaultInstantiationManager();
		this.errorHandlerManager = new DefaultJpcErrorHandler();
		this.refManager = RefManager.getDefaultRefManager();
	}
	
	public Jpc build() {
		return new DefaultJpc(converterManager, typeSolverManager, instantiationManager, refManager, errorHandlerManager);
	}

	public JpcBuilder registerConverter(JpcConverter<?,?> converter) {
		converterManager.register(converter);
		return this;
	}
	
	public JpcBuilder registerTypeSolver(TermTypeSolver typeSolver) {
		typeSolverManager.register(typeSolver);
		return this;
	}
	
	public JpcBuilder registerInstanceCreator(InstanceCreator instanceCreator) {
		instantiationManager.register(instanceCreator);
		return this;
	}
	
	public JpcBuilder registerErrorHandler(ErrorHandler errorHandler) {
		errorHandlerManager.register(errorHandler);
		return this;
	}
	
	public JpcBuilder setRefManager(RefManager refManager) {
		this.refManager = refManager;
		return this;
	}
	
//	//TODO
//	public JpcBuilder setPreferences(JpcPreferences preferences) {
//		throw new NotYetImplementedException();
//	}

}
