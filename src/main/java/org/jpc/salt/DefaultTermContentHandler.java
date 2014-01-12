package org.jpc.salt;



/**
 * Defines do-nothing methods for the Context Handler interface, facilitating in this way the implementation
 * TODO: this class will be removed when Java8 is released since apparently that release will incorporate the notion of Default methods in interfaces
 * @author sergioc
 *
 */
public class DefaultTermContentHandler implements TermContentHandler {

	@Override
	public TermContentHandler startIntegerTerm(long value) {
		return this;
	}

	@Override
	public TermContentHandler startFloatTerm(double value) {
		return this;
	}

	@Override
	public TermContentHandler startVariable(String name) {
		return this;
	}

	@Override
	public TermContentHandler startAtom(String name) {
		return this;
	}

	@Override
	public TermContentHandler startJRef(Object ref) {
		return this;
	}
	
	@Override
	public TermContentHandler startCompound() {
		return this;
	}

	@Override
	public TermContentHandler endCompound() {
		return this;
	}
	

}
