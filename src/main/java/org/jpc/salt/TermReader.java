package org.jpc.salt;

/**
 * A SALT reader
 * @author sergioc
 *
 */
public abstract class TermReader {

	protected ContentHandler contentHandler;
	
//	public TermReader() {
//	}
	
	public TermReader(ContentHandler contentHandler) {
		this.contentHandler = contentHandler;
	}
	
	public ContentHandler getContentHandler() {
		return contentHandler;
	}

	public void setContentHandler(ContentHandler contentHandler) {
		this.contentHandler = contentHandler;
	}
	
	public abstract void read();

}
