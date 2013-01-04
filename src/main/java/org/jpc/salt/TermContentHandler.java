package org.jpc.salt;


public interface TermContentHandler {

	public abstract TermContentHandler startIntegerTerm(long value);
	public abstract TermContentHandler startFloatTerm(double value);
	public abstract TermContentHandler startVariable(String name);
	public abstract TermContentHandler startAtom(String name);
	//compound events
	public abstract TermContentHandler startCompound();
	public abstract TermContentHandler endCompound();

}
