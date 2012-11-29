package org.jpc.salt;


public abstract class ContentHandlerAdapter implements ContentHandler {

	protected ContentHandler contentHandler;
	
	public ContentHandlerAdapter(ContentHandler contentHandler) {
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

	public void startCompoundName() {
		contentHandler.startCompoundName();
	}

	public void endCompoundName() {
		contentHandler.endCompoundName();
	}

	public void startCompoundArg() {
		contentHandler.startCompoundArg();
	}

	public void endCompoundArg() {
		contentHandler.endCompoundArg();
	}

	public void endCompound() {
		contentHandler.endCompound();
	}

}
