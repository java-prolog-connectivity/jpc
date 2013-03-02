package org.jpc.engine.prolog;

import org.jpc.query.Query;
import org.jpc.term.Term;
import org.jpc.term.TermConvertable;

public interface BootstrapPrologEngine {
	
	public abstract boolean stop();
	
	public abstract Query query(TermConvertable termConvertable);
	
	/**
	 * 
	 * @param termString
	 * @return the term representation of a String. Variable names should be preserved.
	 */
	public abstract Term asTerm(String termString);

	/**
	 * escape the given string adding quotes and escaping characters if needed
	 * @param s the string to escape
	 * @return the escaped string
	 */
	public abstract String escape(String s);
}
