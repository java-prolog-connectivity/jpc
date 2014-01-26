package org.jpc.util.config;

import java.util.Collections;
import java.util.Set;

import org.jpc.engine.prolog.driver.PrologEngineFactory;

import com.google.common.base.Preconditions;

public class EngineConfiguration {

	private final String alias;
	private final Set<String> packageNames;
	private final PrologEngineFactory<?> engineFactory;
	
	public EngineConfiguration(String alias, PrologEngineFactory<?> engineFactory) {
		this(alias, Collections.<String>emptySet(), engineFactory);
	}
	
	public EngineConfiguration(Set<String> packageNames, PrologEngineFactory<?> engineFactory) {
		this(null, packageNames, engineFactory);
	}
	
	public EngineConfiguration(String alias, Set<String> packageNames, PrologEngineFactory<?> engineFactory) {
		Preconditions.checkArgument( (alias != null && !alias.trim().isEmpty()) || !packageNames.isEmpty());
		Preconditions.checkNotNull(engineFactory);
		this.alias = alias;
		this.packageNames = packageNames;
		this.engineFactory = engineFactory;
	}

	public String getAlias() {
		return alias;
	}

	public Set<String> getPackageNames() {
		return packageNames;
	}

	public PrologEngineFactory<?> getEngineFactory() {
		return engineFactory;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((alias == null) ? 0 : alias.hashCode());
		result = prime * result
				+ ((engineFactory == null) ? 0 : engineFactory.hashCode());
		result = prime * result
				+ ((packageNames == null) ? 0 : packageNames.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		EngineConfiguration other = (EngineConfiguration) obj;
		if (alias == null) {
			if (other.alias != null)
				return false;
		} else if (!alias.equals(other.alias))
			return false;
		if (engineFactory == null) {
			if (other.engineFactory != null)
				return false;
		} else if (!engineFactory.equals(other.engineFactory))
			return false;
		if (packageNames == null) {
			if (other.packageNames != null)
				return false;
		} else if (!packageNames.equals(other.packageNames))
			return false;
		return true;
	}
	
	
	
	
}
