package org.jpc.salt;

public interface PrologContentHandler extends TermContentHandler {

	//Prolog related events
	public abstract TermContentHandler followingDirectives(); //next read terms will be interpreted as goals to be executed
	public abstract TermContentHandler followingDynamicClauses(); //next read terms will be considered dynamic clauses to be asserted
	
	//Logtalk related events
	public abstract TermContentHandler startLogtalkObjectContext();
	public abstract TermContentHandler endLogtalkObjectContext();
	
}
