package org.jpc;

import static java.util.Collections.emptyList;
import static org.jconverter.converter.ConverterKey.converterKey;

import org.jcategory.JCategory;
import org.jconverter.JConverterBuilder;
import org.jconverter.factory.FactoryManager;
import org.jconverter.factory.FactoryManagerImpl;
import org.jpc.mapping.converter.JpcConverter;
import org.jpc.mapping.converter.JpcConverterManager;
import org.jpc.mapping.typesolver.JpcTypeSolverManager;
import org.jconverter.typesolver.TypeSolver;
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
	 * @param categorization a categorization context.
	 * @return a JpcBuilder.
	 */
	public static JpcBuilder create(JCategory categorization) {
		return new JpcBuilder(categorization);
	}
	
	private JpcBuilder() {
		this(new JCategory());
	}
	
	private JpcBuilder(JCategory categorization) {
		this(categorization, new JpcEngine());
	}
	
	private JpcBuilder(JCategory categorization, JpcEngine embeddedEngine) {
		this(JpcConverterManager.registerDefaults(new JpcConverterManager(categorization, embeddedEngine)),
				FactoryManagerImpl.registerDefaults(new FactoryManagerImpl(categorization)),
				JpcTypeSolverManager.registerDefaults(new JpcTypeSolverManager(categorization, embeddedEngine)),
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

	public JpcBuilder register(Object contextId, JpcConverter converter) {
		getConverterManager().register(converterKey(contextId), converter);
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
		return new JpcImpl(getConverterManager(), factoryManager, emptyList(), typeSolverManager, refTermManager, errorHandlerManager);
	}
	
}
