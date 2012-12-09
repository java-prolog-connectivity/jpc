package org.jpc.salt.contenthandler;

public interface PrologContentHandler extends TermContentHandler {

	public abstract void startPrologDirective();
	public abstract void endPrologDirective();
	
	public abstract void startPrologClause();
	public abstract void endPrologClause();

}
