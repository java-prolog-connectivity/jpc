package org.jpc.salt;

/**
 * The interface of objects interested in listening to the description of logic terms as a stream of events
 * @author sergioc
 *
 */
public interface TermContentHandler {

	public abstract TermContentHandler startIntegerTerm(long value);
	public abstract TermContentHandler startFloatTerm(double value);
	public abstract TermContentHandler startVariable(String name);
	public abstract TermContentHandler startAtom(String name);
	public abstract TermContentHandler startJRef(Object ref);
	public abstract TermContentHandler startSoftJRef(Object ref);
	public abstract TermContentHandler startWeakJRef(Object ref);
	//compound events
	public abstract TermContentHandler startCompound();
	public abstract TermContentHandler endCompound();

}
