package org.jpc.util.engine.supported;

import org.jpc.engine.dialect.Dialect;

public abstract class EngineDescription {

	public abstract String getName();
	
	/**
	 * 
	 * @return the dialect as answered by the query "prolog_flag(dialect, Dialect)".
	 */
	public abstract Dialect getDialect();
	
	public abstract String getDescription();
	
	public abstract String getLicenseUrl();
	
	public abstract String getSiteUrl();
	
}
