package org.jpc;

import org.jpc.converter.ConverterManager;
import org.jpc.converter.DefaultConverterManager;
import org.jpc.converter.JpcConverter;
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
	
	public JpcBuilder() {
		this.converterManager = new DefaultConverterManager();
		this.typeSolverManager = new DefaultTypeSolverManager();
		this.instantiationManager = new DefaultInstantiationManager();
	}
	
	public Jpc create() {
		return new Jpc(converterManager, typeSolverManager, instantiationManager);
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
	
//	//TODO
//	public void setPreferences(JpcPreferences preferences) {
//		throw new NotYetImplementedException();
//	}

}
