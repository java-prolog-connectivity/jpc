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
	public ContentHandler startIntegerTerm(long value) {
		contentHandler.startIntegerTerm(value);
		return this;
	}

	@Override
	public ContentHandler startFloatTerm(double value) {
		contentHandler.startFloatTerm(value);
		return this;
	}

	@Override
	public ContentHandler startVariable(String variableName) {
		contentHandler.startVariable(variableName);
		return this;
	}

	@Override
	public ContentHandler startAtom(String atomName) {
		contentHandler.startAtom(atomName);
		return this;
	}


	public ContentHandler startCompound() {
		contentHandler.startCompound();
		return this;
	}

	public ContentHandler endCompound() {
		contentHandler.endCompound();
		return this;
	}
	
	

	public ContentHandler startPrologDirective() {
		contentHandler.startPrologDirective();
		return this;
	}

	public ContentHandler startPrologClause() {
		contentHandler.startPrologClause();
		return this;
	}

	public ContentHandler startLogtalkObjectClause() {
		contentHandler.startLogtalkObjectClause();
		return this;
	}

}
