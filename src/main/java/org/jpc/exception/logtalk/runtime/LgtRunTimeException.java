package org.jpc.exception.logtalk.runtime;

import org.jpc.exception.logtalk.LogtalkException;
import org.jpc.term.Compound;
import org.jpc.term.Term;
import org.jpc.term.Variable;

public class LgtRunTimeException extends LogtalkException {

	public Term getCall() {
		Compound error = (Compound) getExceptionTerm();
		Compound logtalkArg = (Compound) error.arg(2);
		return logtalkArg.arg(1);
	}
	
	public Term getEntity() {
		Compound error = (Compound) getExceptionTerm();
		Compound logtalkArg = (Compound) error.arg(2);
		return logtalkArg.arg(2);
	}
	
	public LgtRunTimeException(Term exceptionTerm) {
		super(exceptionTerm);
	}
	
	public LgtRunTimeException(Term exceptionTerm, Term goal) {
		super(exceptionTerm, goal);
	}

	@Override
	public String getMessage() {
		StringBuilder sb = new StringBuilder(super.getMessage());
		Term entity = getEntity();
		if(!(entity instanceof Variable))
			sb.append("Logtalk Entity: " + entity);
		Term call = getCall();
		if(!(call instanceof Variable))
			sb.append("Logtalk Call: " + call);
		return sb.toString();
	}
	
}
