package org.jpc.salt;



public interface ContentHandler {

	public abstract ContentHandler startIntegerTerm(long value);
	public abstract ContentHandler startFloatTerm(double value);
	public abstract ContentHandler startVariable(String name);
	public abstract ContentHandler startAtom(String name);
	//compound events
	public abstract ContentHandler startCompound();
	public abstract ContentHandler endCompound();
	
	//Prolog related events
	public abstract ContentHandler startPrologDirective();
	
	public abstract ContentHandler startPrologClause();
	
	//Logtalk related events
	public abstract ContentHandler startLogtalkObjectClause();
}
