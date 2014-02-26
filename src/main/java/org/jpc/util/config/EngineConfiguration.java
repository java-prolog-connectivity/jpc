package org.jpc.util.config;

import java.util.Collections;
import java.util.Set;

import org.jpc.engine.prolog.PrologEngine;
import org.jpc.engine.prolog.driver.PrologEngineFactory;

import com.google.common.base.Preconditions;

public class EngineConfiguration<T extends PrologEngine> {

	private final String name;
	private final Set<String> packageNames;
	private final PrologEngineFactory<T> engineFactory;
	
	public EngineConfiguration(String name, PrologEngineFactory<T> engineFactory) {
		this(name, Collections.<String>emptySet(), engineFactory);
	}
	
	public EngineConfiguration(Set<String> packageNames, PrologEngineFactory<T> engineFactory) {
		this(null, packageNames, engineFactory);
	}
	
	public EngineConfiguration(String name, Set<String> packageNames, PrologEngineFactory<T> engineFactory) {
		Preconditions.checkArgument( name != null || !packageNames.isEmpty());
		Preconditions.checkNotNull(engineFactory);
		this.name = name;
		this.packageNames = packageNames;
		this.engineFactory = engineFactory;
	}

	public Object getName() {
		return name;
	}

	public Set<String> getPackageNames() {
		return packageNames;
	}

	public PrologEngineFactory<T> getEngineFactory() {
		return engineFactory;
	}
	
}
