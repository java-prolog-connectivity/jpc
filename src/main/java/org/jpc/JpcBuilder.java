package org.jpc;

import org.jconverter.JConverterBuilder;
import org.jconverter.factory.FactoryManager;
import org.jconverter.factory.FactoryManagerImpl;
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
import org.jpc.term.refterm.RefTermManager;

public class JpcBuilder extends JConverterBuilder {
	
	private final JpcTypeSolverManager typeSolverManager;
	private RefTermManager refTermManager;
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
				FactoryManagerImpl.registerDefaults(new FactoryManagerImpl(jgum)),
				JpcTypeSolverManager.registerDefaults(new JpcTypeSolverManager(jgum, embeddedEngine)),
				new RefTermManager(), 
				new DefaultJpcErrorHandler());
	}
	
	private JpcBuilder(JpcConverterManager converterManager, FactoryManager factoryManager, JpcTypeSolverManager typeSolverManager, RefTermManager refTermManager, ErrorHandlerManager errorHandlerManager) {
		super(converterManager, factoryManager);
		this.typeSolverManager = typeSolverManager;
		this.refTermManager = refTermManager;
		this.errorHandlerManager = errorHandlerManager;
	}
	
	private JpcConverterManager getConverterManager() {
		return (JpcConverterManager) converterManager;
	}

	/**
	 * Registers a converter.
	 * @param converter the converter to register.
	 * @return this JpcBuilder.
	 */
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
	 * @return this JpcBuilder.
	 */
	public JpcBuilder register(TypeSolver<?> typeSolver) {
		typeSolverManager.register(typeSolver);
		return this;
	}
	
	public JpcBuilder register(TypeSolver<?> typeSolver, Term term) {
		typeSolverManager.register(typeSolver, term);
		return this;
	}
	
	/**
	 * Registers an error handler.
	 * @param errorHandler the error handler to register.
	 * @return this JpcBuilder.
	 */
	public JpcBuilder register(ErrorHandler errorHandler) {
		errorHandlerManager.register(errorHandler);
		return this;
	}
	
	public JpcBuilder setRefTermManager(RefTermManager refTermManager) {
		this.refTermManager = refTermManager;
		return this;
	}
	
//	public JpcBuilder setPreferences(JpcPreferences preferences) {
//		throw new NotYetImplementedException();
//	}
	
	public JpcBuilder configure(JpcConfigurator jpcConfigurator) {
		jpcConfigurator.configure(this);
		return this;
	}
	
	public Jpc build() {
		return new DefaultJpc(getConverterManager(), factoryManager, typeSolverManager, refTermManager, errorHandlerManager);
	}
	
}
