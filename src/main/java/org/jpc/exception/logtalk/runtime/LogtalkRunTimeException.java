package org.jpc.exception.logtalk.runtime;

import org.jpc.exception.logtalk.LogtalkException;
import org.jpc.term.Compound;
import org.jpc.term.Term;
import org.jpc.term.Variable;

/**
 * An exception in the Prolog side having the format "error(FormalDescription, logtalk(Call, Entity))"
 * @author sergioc
 *
 */
public class LogtalkRunTimeException extends LogtalkException {

	public LogtalkRunTimeException(Term exceptionTerm) {
		super(exceptionTerm);
	}
	
	public LogtalkRunTimeException(Term exceptionTerm, Term goal) {
		super(exceptionTerm, goal);
	}
	
	public Term getCall() {
		Compound logtalkArg = (Compound) getContextTerm();
		return logtalkArg.arg(1);
	}
	
	public Term getEntity() {
		Compound logtalkArg = (Compound) getContextTerm();
		return logtalkArg.arg(2);
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
