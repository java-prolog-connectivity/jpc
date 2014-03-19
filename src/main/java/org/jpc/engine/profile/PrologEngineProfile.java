package org.jpc.engine.profile;

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

	private static final Logger logger = LoggerFactory.getLogger(PrologEngineProfile.class);
	
	private PrologEngineFactory<T> prologEngineFactory;

	public PrologEngineProfile(PrologEngineFactory<T> prologEngineFactory) {
		this.prologEngineFactory = prologEngineFactory;
	}

	public PrologEngineFactory<T> getPrologEngineFactory() {
		return prologEngineFactory;
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
