package org.jpc;

import org.jconverter.JConverterBuilder;
import org.jconverter.converter.ConverterManager;
import org.jconverter.instantiation.InstantiationManager;
import org.jconverter.instantiation.JGumInstantiationManager;
import org.jgum.JGum;
import org.jpc.converter.FromTermConverter;
import org.jpc.converter.FromTermConverterAdapter;
import org.jpc.converter.JpcConverter;
import org.jpc.converter.JpcConverterManager;
import org.jpc.converter.ToTermConverter;
import org.jpc.converter.ToTermConverterAdapter;
import org.jpc.converter.typesolver.JGumTypeSolverManager;
import org.jpc.converter.typesolver.TypeSolver;
import org.jpc.converter.typesolver.TypeSolverManager;
import org.jpc.error.handling.DefaultJpcErrorHandler;
import org.jpc.error.handling.ErrorHandler;
import org.jpc.error.handling.ErrorHandlerManager;
import org.jpc.term.jterm.JTermManager;
import org.jpc.term.jterm.JTermUtil;

public class JpcBuilder extends JConverterBuilder {

	//TODO move these methods to JpcConverter with Java8 (since then static methods will be allowed in interfaces)
	public static void register(ConverterManager converterManager, JpcConverter converter) {
		if(converter instanceof FromTermConverter)
			register(converterManager, (FromTermConverter)converter);
		if(converter instanceof ToTermConverter)
			register(converterManager, (ToTermConverter)converter);
	}
	
	private static void register(ConverterManager converterManager, FromTermConverter<?,?> converter) {
		converterManager.register(FromTermConverterAdapter.forConverter(converter));
	}
	
	private static void register(ConverterManager converterManager, ToTermConverter<?,?> converter) {
		converterManager.register(ToTermConverterAdapter.forConverter(converter));
	}
	
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
				JTermUtil.getJTermManager(), 
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
		register(converterManager, converter);
		return this;
	}
	
	
//	private JpcBuilder register(FromTermConverter<?,?> converter) {
//		register(converterManager, converter);
//		return this;
//	}
//	
//	private JpcBuilder register(ToJTermTermConverter<?,?> converter) {
//		register(converterManager, converter);
//		return this;
//	}
	
	public JpcBuilder setRefManager(JTermManager jTermManager) {
		this.jTermManager = jTermManager;
		return this;
	}
	
	public JpcBuilder registerErrorHandler(ErrorHandler errorHandler) {
		errorHandlerManager.register(errorHandler);
		return this;
	}
	

//	public JpcBuilder setPreferences(JpcPreferences preferences) {
//		throw new NotYetImplementedException();
//	}

	
	/**
	 * Registers a type solver.
	 * @param typeSolver the type solver to register.
	 */
	public void register(TypeSolver typeSolver) {
		typeSolverManager.register(typeSolver);
	}
	
	/**
	 * Registers a type solver under a given key. 
	 * @param key the key under which the instance creator is registered.
	 * @param typeSolver the type solver to register.
	 */
	public void register(Object key, TypeSolver typeSolver) {
		typeSolverManager.register(key, typeSolver);
	}
	
}
