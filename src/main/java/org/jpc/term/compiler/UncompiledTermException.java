package org.jpc.term.compiler;

import org.jpc.JpcException;
import org.jpc.term.Term;

public class UncompiledTermException extends JpcException {

	private final Term term;
	
	public UncompiledTermException(Term term) {
		this.term = term;
	}
	
	@Override
	public String getMessage() {
		return "Attempt to execute an operation requirying a compiled term with: " + term + ".";
	}
}
