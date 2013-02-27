package org.jpc.util.concurrent;

import java.util.concurrent.Executors;

import org.jpc.engine.prolog.configuration.PrologEngineConfiguration;
import org.minitoolbox.concurrent.OneThreadFactory;

public class OneThreadJpcExecutor extends JpcExecutor {

	public OneThreadJpcExecutor(PrologEngineConfiguration prologEngineConfiguration) {
		super(Executors.newSingleThreadExecutor(new OneThreadFactory()), prologEngineConfiguration);
	}

}
