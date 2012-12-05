package org.jpc.salt;


/**
 * Defines do-nothing methods for the Context Handler interface, facilitating in this way the implementation
 * TODO: this class will be removed when Java8 is released since apparently that release will incorporate the notion of Default methods in interfaces
 * @author sergioc
 *
 */
public class DefaultHandler implements ContentHandler {

	@Override
	public void startIntegerTerm(long value) {
	}

	@Override
	public void startFloatTerm(double value) {
	}

	@Override
	public void startVariable(String name) {
	}

	@Override
	public void startAtom(String name) {
	}

	@Override
	public void startCompoundName() {
	}

	@Override
	public void startCompoundArgs() {
	}

	@Override
	public void startCompoundArg() {
	}

	@Override
	public void endCompoundArg() {
	}

	@Override
	public void endCompoundArgs() {
	}

	
}
