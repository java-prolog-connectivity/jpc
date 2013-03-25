package org.jpc.salt;

public interface PrologContentHandler extends TermContentHandler {

	//Prolog related events
	public abstract TermContentHandler followingDirectives(); //next read terms will be interpreted as goals to be executed
	public abstract TermContentHandler followingDynamicClauses(); //next read terms will be considered dynamic clauses to be asserted
	
	//Logtalk related events
	/**
	 * 
	 * The next term to come represents the Logtalk context object.
	 * All the following directives and dynamic clauses are executed in the context of this object until a call to endLogtalkObjectContext()
	 * @return
	 */
	public abstract TermContentHandler startLogtalkObjectContext();
	public abstract TermContentHandler endLogtalkObjectContext();
	
}
