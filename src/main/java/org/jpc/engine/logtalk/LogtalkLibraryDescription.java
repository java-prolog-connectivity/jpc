package org.jpc.engine.logtalk;

import java.io.File;
import java.net.URI;

import org.jpc.term.Term;

public class LogtalkLibraryDescription {

	private String alias;
	private Term termPath;
	private URI dirUri; //the URI of the file
	
	public LogtalkLibraryDescription(String alias, Term termPath, File dirFile) {
		this(alias, termPath, dirFile.toURI());
	}
	
	public LogtalkLibraryDescription(String alias, Term termPath, URI dirUri) {
		this.alias = alias;
		this.termPath = termPath;
		this.dirUri = dirUri;
	}

	public String getAlias() {
		return alias;
	}

	public Term getTermPath() {
		return termPath;
	}

	public URI getDirUri() {
		return dirUri;
	}

}
