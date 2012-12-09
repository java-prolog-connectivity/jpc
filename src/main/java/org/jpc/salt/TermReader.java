package org.jpc.salt;

import org.jpc.salt.contenthandler.TermContentHandler;

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

	public void setContentHandler(TermContentHandler contentHandler) {
		this.contentHandler = contentHandler;
	}
	
	public abstract void read();

}
