package org.jpc.util.config;

import java.util.Collections;
import java.util.Set;

import org.jpc.engine.prolog.PrologEngine;
import org.jpc.engine.prolog.driver.PrologEngineFactory;

import com.google.common.base.Preconditions;

public class EngineConfiguration<T extends PrologEngine> {

	/**
	 * The id of this engine configuration.
	 */
	private final Object id;
	
	/**
	 * A set of named categories to which this configuration applies.
	 */
	private final Set<String> namedCategories;
	
	/**
	 * An engine factory creating Prolog engines specified by this configuration.
	 */
	private final PrologEngineFactory<T> engineFactory;
	
	public EngineConfiguration(Object id, PrologEngineFactory<T> engineFactory) {
		this(id, Collections.<String>emptySet(), engineFactory);
	}
	
	public EngineConfiguration(Set<String> namedCategories, PrologEngineFactory<T> engineFactory) {
		this(null, namedCategories, engineFactory);
	}
	
	public EngineConfiguration(Object id, Set<String> namedCategories, PrologEngineFactory<T> engineFactory) {
		Preconditions.checkArgument( id != null || !namedCategories.isEmpty());
		Preconditions.checkNotNull(engineFactory);
		this.id = id;
		this.namedCategories = namedCategories;
		this.engineFactory = engineFactory;
	}

	/**
	 * 
	 * @return the id of this engine configuration.
	 */
	public Object getId() {
		return id;
	}

	/**
	 * 
	 * @return the set of named categories to which this configuration applies.
	 */
	public Set<String> getNamedCategories() {
		return namedCategories;
	}

	/**
	 * 
	 * @return the engine factory creating Prolog engines specified by this configuration.
	 */
	public PrologEngineFactory<T> getEngineFactory() {
		return engineFactory;
	}
	
}
