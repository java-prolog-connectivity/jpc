package org.jpc.salt;

/**
 * A SALT reader
 * @author sergioc
 *
 */
public abstract class TermReader implements ContentHandler {

	protected ContentHandler contentHandler;
	
	public TermReader(ContentHandler contentHandler) {
		this.contentHandler = contentHandler;
	}
	
	public ContentHandler getContentHandler() {
		return contentHandler;
	}

	public void startIntegerTerm(long value) {
		contentHandler.startIntegerTerm(value);
	}

	public void startFloatTerm(double value) {
		contentHandler.startFloatTerm(value);
	}

	public void startVariable(String variableName) {
		contentHandler.startVariable(variableName);
	}

	public void startAtom(String atomName) {
		contentHandler.startAtom(atomName);
	}

	public void startCompound() {
		contentHandler.startCompound();
	}

	public void endCompound() {
		contentHandler.endCompound();
	}

}
