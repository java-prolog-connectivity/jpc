package org.jpc;

import org.jconverter.JConverterBuilder;
import org.jconverter.instantiation.InstantiationManager;
import org.jconverter.instantiation.JGumInstantiationManager;
import org.jgum.JGum;
import org.jpc.converter.JpcConverter;
import org.jpc.converter.JpcConverterManager;
import org.jpc.converter.typesolver.JGumTypeSolverManager;
import org.jpc.converter.typesolver.TypeSolver;
import org.jpc.converter.typesolver.TypeSolverManager;
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
		this(new JGum());
	}
	
	private JpcBuilder(JGum jgum) {
		this(JpcConverterManager.createDefault(jgum),
				JGumInstantiationManager.createDefault(jgum), 
				JGumTypeSolverManager.createDefault(jgum), 
				new JTermManager(), 
				new DefaultJpcErrorHandler());
	}
	
	protected JpcBuilder(JpcConverterManager converterManager, InstantiationManager instantiationManager, TypeSolverManager typeSolverManager, JTermManager jTermManager, ErrorHandlerManager errorHandlerManager) {
		super(converterManager, instantiationManager);
		this.typeSolverManager = typeSolverManager;
		this.jTermManager = jTermManager;
		this.errorHandlerManager = errorHandlerManager;
	}
	
	public Jpc build() {
		return new DefaultJpc((JpcConverterManager)converterManager, instantiationManager, typeSolverManager, jTermManager, errorHandlerManager);
	}

	public JpcBuilder register(JpcConverter converter) {
		JpcConverterManager.register(converterManager, converter);
		return this;
	}
	
	public JpcBuilder register(JpcConverter converter, Term term) {
		((JpcConverterManager)converterManager).register(converter, term);
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
