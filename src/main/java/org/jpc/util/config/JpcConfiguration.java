package org.jpc.util.config;

import java.util.Set;

import org.jpc.engine.prolog.PrologEngine;

import com.google.common.base.Preconditions;

public class JpcConfiguration {

	private final Set<EngineConfiguration<? extends PrologEngine>> engineConfigurations;
	
	public JpcConfiguration(Set<EngineConfiguration<? extends PrologEngine>> engineConfigurations) {
		Preconditions.checkNotNull(engineConfigurations);
		this.engineConfigurations = engineConfigurations;
	}

	public Set<EngineConfiguration<? extends PrologEngine>> getEngineConfigurations() {
		return engineConfigurations;
	}

}
