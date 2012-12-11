package org.jpc.salt;



/**
 * Defines do-nothing methods for the Context Handler interface, facilitating in this way the implementation
 * TODO: this class will be removed when Java8 is released since apparently that release will incorporate the notion of Default methods in interfaces
 * @author sergioc
 *
 */
public class DefaultContentHandler implements ContentHandler {

	@Override
	public ContentHandler startIntegerTerm(long value) {
		return this;
	}

	@Override
	public ContentHandler startFloatTerm(double value) {
		return this;
	}

	@Override
	public ContentHandler startVariable(String name) {
		return this;
	}

	@Override
	public ContentHandler startAtom(String name) {
		return this;
	}

	@Override
	public ContentHandler startCompound() {
		return this;
	}

	@Override
	public ContentHandler endCompound() {
		return this;
	}
	
	
	@Override
	public ContentHandler startPrologDirective() {
		return this;
	}

	@Override
	public ContentHandler startPrologClause() {
		return this;
	}
	
	
	@Override
	public ContentHandler startLogtalkObjectClause() {
		return this;
	}

}
