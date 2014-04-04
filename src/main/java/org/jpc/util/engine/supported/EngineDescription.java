package org.jpc.util.engine.supported;

public abstract class EngineDescription {

	public abstract String getName();
	
	/**
	 * 
	 * @return the dialect as answered by the query "prolog_flag(dialect, Dialect)".
	 */
	public abstract String getDialect();
	
	public abstract String getDescription();
	
	public abstract String getLicenseUrl();
	
	public abstract String getSiteUrl();
	
}
