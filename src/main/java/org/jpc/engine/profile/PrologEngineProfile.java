package org.jpc.engine.profile;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.jpc.engine.prolog.PrologEngine;
import org.jpc.engine.prolog.driver.PrologEngineFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * A factory of tailored Prolog engines.
 * The objective of this class is to decouple the work of creating a Prolog engine (task of drivers) from the work of customizing it.
 * Different profiles can initialize Prolog engines according to certain required features (e.g., configuration of Logtalk, preloading of files, etc).
 * @author sergioc
 *
 */
public class PrologEngineProfile<T extends PrologEngine> implements PrologEngineFactory<T> {

	private static Logger logger = LoggerFactory.getLogger(PrologEngineProfile.class);
	
	private PrologEngineFactory<T> prologEngineFactory;
	private String name; //This optional attribute is intended to be used for GUI development in a multi-engine environment.
	
	//(Experimental) The scope (packages) where this logic engine should be applied. This attribute may be used for automatic configuration of Prolog Engines 
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

	public boolean isDisabled() {
		return prologEngineFactory.isDisabled();
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
	public final T createPrologEngine() {
		T prologEngine = basicCreatePrologEngine();
		try {
			onCreate(prologEngine);
		} catch(RuntimeException e) {
			if(prologEngine.isCloseable())
				prologEngine.close();
			throw e;
		}
		return prologEngine;
	}
	
	/**
	 * 
	 * @return a instance of a Prolog engine.
	 */
	protected final T basicCreatePrologEngine() {
		return prologEngineFactory.createPrologEngine();
	}
	
	public void onCreate(T prologEngine) {
		//nothing by default, to be overridden if needed
	}
}
