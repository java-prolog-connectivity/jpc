package org.jpc.engine.prolog;

import org.jpc.term.Term;
import org.jpc.term.TermConvertable;


public abstract class BootstrapPrologEngine {

	public abstract Query createQuery(TermConvertable termConvertable);
	
	public boolean stop() {
		throw new UnsupportedOperationException();
	}
	
	/**
	 * 
	 * @param termString
	 * @return the term representation of a String. Variable names should be preserved.
	 */
	public Term asTerm(String termString) {
		throw new UnsupportedOperationException();
	}

	/**
	 * escape the given string adding quotes and escaping characters if needed
	 * @param s the string to escape
	 * @return the escaped string
	 */
	public String escape(String s) {
		throw new UnsupportedOperationException();
	}
	
}
