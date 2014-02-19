package org.jpc;

import org.jconverter.JConverterBuilder;
import org.jconverter.factory.FactoryManager;
import org.jconverter.factory.JGumFactoryManager;
import org.jgum.JGum;
import org.jpc.converter.JpcConverter;
import org.jpc.converter.JpcConverterManager;
import org.jpc.converter.typesolver.JGumTypeSolverManager;
import org.jpc.converter.typesolver.JpcTypeSolverManager;
import org.jpc.converter.typesolver.TypeSolver;
import org.jpc.converter.typesolver.TypeSolverManager;
import org.jpc.engine.embedded.JpcEngine;
import org.jpc.error.handling.DefaultJpcErrorHandler;
import org.jpc.error.handling.ErrorHandler;
import org.jpc.error.handling.ErrorHandlerManager;
import org.jpc.term.Term;
import org.jpc.term.jterm.JTermManager;

public class JpcBuilder extends JConverterBuilder {
	
	private final TypeSolverManager typeSolverManager;
	private JTermManager jTermManager;
	private final ErrorHandlerManager errorHandlerManager;
	
	
	
	public static JpcBuilder create() {
		return new JpcBuilder();
	}
	
	private JpcBuilder() {
		this(new JGum(), new JpcEngine());
	}
	
	private JpcBuilder(JGum jgum, JpcEngine embeddedEngine) {
		this(JpcConverterManager.registerDefaults(new JpcConverterManager(jgum, embeddedEngine)),
				JGumFactoryManager.registerDefaults(new JGumFactoryManager(jgum)),
				JpcTypeSolverManager.registerDefaults(new JpcTypeSolverManager(jgum, embeddedEngine)),
				new JTermManager(), 
				new DefaultJpcErrorHandler());
	}
	
	private JpcBuilder(JpcConverterManager converterManager, FactoryManager factoryManager, JpcTypeSolverManager typeSolverManager, JTermManager jTermManager, ErrorHandlerManager errorHandlerManager) {
		super(converterManager, factoryManager);
		this.typeSolverManager = typeSolverManager;
		this.jTermManager = jTermManager;
		this.errorHandlerManager = errorHandlerManager;
	}
	
	private JpcConverterManager getConverterManager() {
		return (JpcConverterManager) converterManager;
	}
	
	public Jpc build() {
		return new DefaultJpc(getConverterManager(), factoryManager, typeSolverManager, jTermManager, errorHandlerManager);
	}

	public JpcBuilder register(JpcConverter converter) {
		getConverterManager().register(converter);
		return this;
	}
	
	public JpcBuilder register(JpcConverter converter, Term term) {
		getConverterManager().register(converter, term);
		return this;
	}
	
	/**
	 * Registers a type solver.
	 * @param typeSolver the type solver to register.
	 */
	public void register(TypeSolver<?> typeSolver) {
		typeSolverManager.register(typeSolver);
	}
	
	public JpcBuilder register(ErrorHandler errorHandler) {
		errorHandlerManager.register(errorHandler);
		return this;
	}
	
	public JpcBuilder setJTermManager(JTermManager jTermManager) {
		this.jTermManager = jTermManager;
		return this;
	}
	

//	public JpcBuilder setPreferences(JpcPreferences preferences) {
//		throw new NotYetImplementedException();
//	}
	
}
