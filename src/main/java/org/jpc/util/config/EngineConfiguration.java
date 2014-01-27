package org.jpc.util.config;

import java.util.Collections;
import java.util.Set;

import org.jpc.engine.prolog.PrologEngine;
import org.jpc.engine.prolog.driver.PrologEngineFactory;

import com.google.common.base.Preconditions;

public class EngineConfiguration<T extends PrologEngine> {

	private final Object alias;
	private final Set<String> packageNames;
	private final PrologEngineFactory<T> engineFactory;
	
	public EngineConfiguration(Object alias, PrologEngineFactory<T> engineFactory) {
		this(alias, Collections.<String>emptySet(), engineFactory);
	}
	
	public EngineConfiguration(Set<String> packageNames, PrologEngineFactory<T> engineFactory) {
		this(null, packageNames, engineFactory);
	}
	
	public EngineConfiguration(Object alias, Set<String> packageNames, PrologEngineFactory<T> engineFactory) {
		Preconditions.checkArgument( alias != null || !packageNames.isEmpty());
		Preconditions.checkNotNull(engineFactory);
		this.alias = alias;
		this.packageNames = packageNames;
		this.engineFactory = engineFactory;
	}

	public Object getAlias() {
		return alias;
	}

	public Set<String> getPackageNames() {
		return packageNames;
	}

	public PrologEngineFactory<T> getEngineFactory() {
		return engineFactory;
	}
	
}
