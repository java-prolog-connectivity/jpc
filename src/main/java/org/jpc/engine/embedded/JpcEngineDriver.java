package org.jpc.engine.embedded;

import org.jpc.engine.prolog.driver.AbstractPrologEngineDriver;
import org.jpc.engine.prolog.driver.PrologEngineFactory;
import org.jpc.util.JpcPreferences;
import org.jpc.util.engine.supported.JpcEmbedded;

public class JpcEngineDriver extends AbstractPrologEngineDriver<JpcEngine> {

	public JpcEngineDriver() {
		super(new JpcEmbedded());
	}
	
	@Override
	protected PrologEngineFactory<JpcEngine> defaultBasicFactory() {
		return new PrologEngineFactory<JpcEngine>() {
			@Override
			public JpcEngine createPrologEngine() {
				return new JpcEngine();
			}
		};
	}

	@Override
	public String getLibraryName() {
		return JpcPreferences.JPC_SHORT_NAME + " embedded";
	}

	@Override
	public boolean isDisabled() {
		return true; //the embedded JPC Prolog database is not currently intended to be directly used by a programmer.
	}
	
}
