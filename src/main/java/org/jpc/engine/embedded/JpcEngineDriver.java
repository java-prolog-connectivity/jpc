package org.jpc.engine.embedded;

import org.jpc.engine.prolog.driver.AbstractPrologEngineDriver;
import org.jpc.util.JpcPreferences;
import org.jpc.util.engine.supported.EngineDescription;
import org.jpc.util.engine.supported.JpcEmbedded;

public class JpcEngineDriver extends AbstractPrologEngineDriver<JpcEngine> {

	@Override
	protected JpcEngine basicCreatePrologEngine() {
		return new JpcEngine();
	}

	@Override
	public String getLibraryName() {
		return JpcPreferences.JPC_SHORT_NAME + " embedded";
	}

	@Override
	public EngineDescription getEngineDescription() {
		return new JpcEmbedded();
	}

	@Override
	public boolean isDisabled() {
		return true; //the embedded JPC Prolog database is not currently intended to be directly used by a programmer.
	}
	
}
