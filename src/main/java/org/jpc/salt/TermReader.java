package org.jpc.salt;


/**
 * A SALT reader
 * @author sergioc
 *
 */
public abstract class TermReader {

	protected TermContentHandler contentHandler;
	
//	public TermReader() {
//	}
	
	public TermReader(TermContentHandler contentHandler) {
		this.contentHandler = contentHandler;
	}
	
	public TermContentHandler getContentHandler() {
		return contentHandler;
	}
	
	public abstract void read();

}
