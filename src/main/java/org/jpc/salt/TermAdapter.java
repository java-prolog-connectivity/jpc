package org.jpc.salt;

/**
 * A SALT adapter
 * @author sergioc
 *
 */
public class TermAdapter implements ContentHandler {

	protected ContentHandler contentHandler;

	public TermAdapter(ContentHandler contentHandler) {
		this.contentHandler = contentHandler;
	}
	
	public ContentHandler getContentHandler() {
		return contentHandler;
	}


	@Override
	public void startIntegerTerm(long value) {
		contentHandler.startIntegerTerm(value);
	}

	@Override
	public void startFloatTerm(double value) {
		contentHandler.startFloatTerm(value);
	}

	@Override
	public void startVariable(String variableName) {
		contentHandler.startVariable(variableName);
	}

	@Override
	public void startAtom(String atomName) {
		contentHandler.startAtom(atomName);
	}


	public void startCompoundName() {
		contentHandler.startCompoundName();
	}

	public void startCompoundArgs() {
		contentHandler.startCompoundArgs();
	}

	public void startCompoundArg() {
		contentHandler.startCompoundArg();
	}

	public void endCompoundArg() {
		contentHandler.endCompoundArg();
	}

	public void endCompoundArgs() {
		contentHandler.endCompoundArgs();
	}
	
}
