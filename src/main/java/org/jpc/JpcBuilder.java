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

	public void registerConverter(JpcConverter<?,?> converter) {
		converterManager.register(converter);
	}
	
	public void registerTypeSolver(TermTypeSolver typeSolver) {
		typeSolverManager.register(typeSolver);
	}
	
	public <CT> void registerInstantiationManager(Class<CT> clazz, Factory<CT> factory) {
		instantiationManager.register(clazz, factory);
	}
	
//	//TODO
//	public void setPreferences(JpcPreferences preferences) {
//		throw new NotYetImplementedException();
//	}

}
