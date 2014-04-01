package org.jpc;

import org.jconverter.JConverterBuilder;
import org.jconverter.factory.FactoryManager;
import org.jconverter.factory.JGumFactoryManager;
import org.jgum.JGum;
import org.jpc.converter.JpcConverter;
import org.jpc.converter.JpcConverterManager;
import org.jpc.converter.typesolver.JpcTypeSolverManager;
import org.jpc.converter.typesolver.TypeSolver;
import org.jpc.engine.embedded.JpcEngine;
import org.jpc.error.handling.DefaultJpcErrorHandler;
import org.jpc.error.handling.ErrorHandler;
import org.jpc.error.handling.ErrorHandlerManager;
import org.jpc.term.Term;
import org.jpc.term.jrefterm.JRefTermManager;

public class JpcBuilder extends JConverterBuilder {
	
	private final JpcTypeSolverManager typeSolverManager;
	private JRefTermManager jRefTermManager;
	private final ErrorHandlerManager errorHandlerManager;
	
	/**
	 * @return a JpcBuilder.
	 */
	public static JpcBuilder create() {
		return new JpcBuilder();
	}
	
	/**
	 * @param jgum a categorization context.
	 * @return a JpcBuilder.
	 */
	public static JpcBuilder create(JGum jgum) {
		return new JpcBuilder(jgum);
	}
	
	private JpcBuilder() {
		this(new JGum());
	}
	
	private JpcBuilder(JGum jgum) {
		this(jgum, new JpcEngine());
	}
	
	private JpcBuilder(JGum jgum, JpcEngine embeddedEngine) {
		this(JpcConverterManager.registerDefaults(new JpcConverterManager(jgum, embeddedEngine)),
				JGumFactoryManager.registerDefaults(new JGumFactoryManager(jgum)),
				JpcTypeSolverManager.registerDefaults(new JpcTypeSolverManager(jgum, embeddedEngine)),
				new JRefTermManager(), 
				new DefaultJpcErrorHandler());
	}
	
	private JpcBuilder(JpcConverterManager converterManager, FactoryManager factoryManager, JpcTypeSolverManager typeSolverManager, JRefTermManager jRefTermManager, ErrorHandlerManager errorHandlerManager) {
		super(converterManager, factoryManager);
		this.typeSolverManager = typeSolverManager;
		this.jRefTermManager = jRefTermManager;
		this.errorHandlerManager = errorHandlerManager;
	}
	
	private JpcConverterManager getConverterManager() {
		return (JpcConverterManager) converterManager;
	}
	
	public Jpc build() {
		return new DefaultJpc(getConverterManager(), factoryManager, typeSolverManager, jRefTermManager, errorHandlerManager);
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
	
	public void register(TypeSolver<?> typeSolver, Term term) {
		typeSolverManager.register(typeSolver, term);
	}
	
	public JpcBuilder register(ErrorHandler errorHandler) {
		errorHandlerManager.register(errorHandler);
		return this;
	}
	
	public JpcBuilder setJRefTermManager(JRefTermManager jRefTermManager) {
		this.jRefTermManager = jRefTermManager;
		return this;
	}
	

//	public JpcBuilder setPreferences(JpcPreferences preferences) {
//		throw new NotYetImplementedException();
//	}
	
}
