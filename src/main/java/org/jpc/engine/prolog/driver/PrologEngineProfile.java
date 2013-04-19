package org.jpc.engine.prolog.driver;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.jpc.engine.prolog.PrologEngine;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class PrologEngineProfile<T extends PrologEngine> implements PrologEngineFactory<T> {

	private static Logger logger = LoggerFactory.getLogger(PrologEngineProfile.class);
	
	private PrologEngineFactory<T> prologEngineFactory;
	private String name; //This optional attribute is intended to be used for GUI development in a multi-engine environment.
	
	//the scope (packages) where this logic engine should be applied. This attribute may be used for automatic configuration of Prolog Engines 
	//TODO consider changing it for an annotation in the configuration class.
	protected List<String> scope; 
	

	public PrologEngineProfile(PrologEngineFactory<T> prologEngineFactory) {
		this.prologEngineFactory = prologEngineFactory;
		scope = new ArrayList<String>();
		//addScope(""); //the root package
	}

	public PrologEngineFactory<T> getPrologEngineFactory() {
		return prologEngineFactory;
	}

	public boolean isEnabled() {
		return prologEngineFactory.isEnabled();
	}
	
	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}
	
	public List<String> getScope() {
		return scope;
	}

	public void setScope(List<String> scope) {
		this.scope = scope;
	}

	public void setScope(String ...newScopes) {
		scope = new ArrayList<>(Arrays.asList(newScopes));
	}
	
	public void addScope(String ...newScopes) {
		scope.addAll(Arrays.asList(newScopes));
	}
	
	@Override
	public T createPrologEngine() {
		T prologEngine = basicCreatePrologEngine();
		onCreate(prologEngine);
		return prologEngine;
	}
	
	
	/**
	 * 
	 * @return a instance of a Prolog engine.
	 */
	protected T basicCreatePrologEngine() {
		return prologEngineFactory.createPrologEngine();
	}
	
	public void onCreate(PrologEngine prologEngine) {
		//nothing by default, to be overridden if needed
	}
}
