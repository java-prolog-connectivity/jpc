package org.jpc.util.salt;


/**
 * A SALT adapter
 * @author sergioc
 *
 */
public class TermAdapter implements TermContentHandler {

	protected TermContentHandler contentHandler;

	public TermAdapter(TermContentHandler contentHandler) {
		this.contentHandler = contentHandler;
	}
	
	public TermContentHandler getContentHandler() {
		return contentHandler;
	}

	@Override
	public TermContentHandler startIntegerTerm(long value) {
		contentHandler.startIntegerTerm(value);
		return this;
	}

	@Override
	public TermContentHandler startFloatTerm(double value) {
		contentHandler.startFloatTerm(value);
		return this;
	}

	@Override
	public TermContentHandler startVariable(String variableName) {
		contentHandler.startVariable(variableName);
		return this;
	}

	@Override
	public TermContentHandler startAtom(String atomName) {
		contentHandler.startAtom(atomName);
		return this;
	}

	@Override
	public TermContentHandler startJRef(Object ref) {
		contentHandler.startJRef(ref);
		return this;
	}
	
	public TermContentHandler startCompound() {
		contentHandler.startCompound();
		return this;
	}

	public TermContentHandler endCompound() {
		contentHandler.endCompound();
		return this;
	}


//	public TermContentHandler startDirective() {
//		contentHandler.startDirective();
//		return this;
//	}
//
//	public TermContentHandler startClause() {
//		contentHandler.startClause();
//		return this;
//	}
//
//	public TermContentHandler startLogtalkObjectContext() {
//		contentHandler.startLogtalkObjectContext();
//		return this;
//	}
//
//	@Override
//	public TermContentHandler endLogtalkObjectContext() {
//		contentHandler.endLogtalkObjectContext();
//		return this;
//	}

}
