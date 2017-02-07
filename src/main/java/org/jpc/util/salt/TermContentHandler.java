package org.jpc.util.salt;

/**
 * The interface of objects interested in listening to the description of logic terms as a stream of events
 * @author sergioc
 *
 */
public interface TermContentHandler {

	default TermContentHandler startIntegerTerm(long value)  {
		return this;
	}

	default TermContentHandler startFloatTerm(double value)  {
		return this;
	}

	default TermContentHandler startVariable(String name)   {
		return this;
	}

	default TermContentHandler startAtom(String name)   {
		return this;
	}

	default TermContentHandler startJRef(Object ref)   {
		return this;
	}

	default TermContentHandler startCompound(String name) {
		return startCompound().startAtom(name);
	}

	//compound events
	default TermContentHandler startCompound()   {
		return this;
	}

	default TermContentHandler endCompound()   {
		return this;
	}

}
