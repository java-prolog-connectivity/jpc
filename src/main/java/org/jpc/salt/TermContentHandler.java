package org.jpc.salt;

/**
 * The interface of objects interested in listening to the description of logic terms as a stream of events
 * @author sergioc
 *
 */
public interface TermContentHandler {

	TermContentHandler startIntegerTerm(long value);
	TermContentHandler startFloatTerm(double value);
	TermContentHandler startVariable(String name);
	TermContentHandler startAtom(String name);
	TermContentHandler startJRef(Object ref);
	//compound events
	TermContentHandler startCompound();
	TermContentHandler endCompound();

}
