package org.jpc.util.config;

import java.util.Set;

import com.google.common.base.Preconditions;

public class JpcConfiguration {

	private final Set<EngineConfiguration> engineConfigurations;
	
	public JpcConfiguration(Set<EngineConfiguration> engineConfigurations) {
		Preconditions.checkNotNull(engineConfigurations);
		this.engineConfigurations = engineConfigurations;
	}

	public Set<EngineConfiguration> getEngineConfigurations() {
		return engineConfigurations;
	}

}
