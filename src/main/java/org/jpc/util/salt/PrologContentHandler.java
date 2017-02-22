package org.jpc.util.salt;

import org.jpc.util.termprocessor.PrologWriter;

public class PrologContentHandler extends JpcTermStreamer {

	public PrologContentHandler(PrologWriter prologWriter) {
		super(prologWriter);
	}

	private PrologWriter getPrologWriter() {
		return (PrologWriter) termProcessor;
	}

	//Prolog related events
	public PrologContentHandler followingDirectives() {
		getPrologWriter().followingDirectives();
		return this;
	}

	public PrologContentHandler followingClauses() {
		getPrologWriter().followingClauses();
		return this;
	}
	
	//Logtalk related events
	/**
	 * 
	 * The next term to come represents the Logtalk context object.
	 * All the following directives and dynamic clauses are executed in the context of this object until a call to endLogtalkObjectContext()
	 * @return
	 */
	public PrologContentHandler startLogtalkObjectContext() {
		getPrologWriter().startLogtalkObjectContext();
		return this;
	}

	public PrologContentHandler endLogtalkObjectContext() {
		getPrologWriter().endLogtalkObjectContext();
		return this;
	}

	public PrologContentHandler startIntegerTerm(long value)  {
		super.startIntegerTerm(value);
		return this;
	}

	public PrologContentHandler startFloatTerm(double value)  {
		super.startFloatTerm(value);
		return this;
	}

	public PrologContentHandler startVariable(String name)   {
		super.startVariable(name);
		return this;
	}

	public PrologContentHandler startAtom(String name)   {
		super.startAtom(name);
		return this;
	}

	public PrologContentHandler startJRef(Object ref)   {
		super.startJRef(ref);
		return this;
	}

	public PrologContentHandler startCompound(String name)   {
		super.startCompound(name);
		return this;
	}

	public PrologContentHandler startCompound()   {
		super.startCompound();
		return this;
	}

	public PrologContentHandler endCompound()   {
		super.endCompound();
		return this;
	}
}
