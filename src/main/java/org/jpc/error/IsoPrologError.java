package org.jpc.error;

import static org.jpc.engine.logtalk.LogtalkConstants.LOGTALK_ERROR_CONTEXT_FUNCTOR;

import org.jpc.term.Compound;
import org.jpc.term.Term;
import org.jpc.term.Var;

/**
 * An exception in the Prolog side having the format "error(FormalDescription, Context)"
 * @author sergioc
 *
 */
public class IsoPrologError extends PrologError {
	
	public IsoPrologError(Compound exceptionTerm) {
		super(exceptionTerm);
	}
	
	public Term getFormalDescriptionTerm() {
		Compound error = (Compound) asTerm();
		return error.arg(1);
	}
	
	public Term getContextTerm() {
		Compound error = (Compound) asTerm();
		return error.arg(2);
	}
	
	@Override
	public String getMessage() {
		StringBuilder sb = new StringBuilder(super.getMessage());
		sb.append("Formal description term: " + getFormalDescriptionTerm());
		Term contextTerm = getContextTerm();
		sb.append(". Context term: " + contextTerm);
		sb.append(". ");
		
		Term logtalkEntity = getLogtalkEntity();
		if(logtalkEntity != null)
			sb.append("Logtalk Entity: " + logtalkEntity + ". ");
		Term logtalkCall = getLogtalkCall();
		if(logtalkCall != null)
			sb.append("Logtalk Call: " + logtalkCall + ". ");
		
		return sb.toString();
	}
	
	
	//Logtalk related methods
	
	public boolean hasLogtalkContext() {
		return getContextTerm().hasFunctor(LOGTALK_ERROR_CONTEXT_FUNCTOR, 2);
	}
	
	public Term getLogtalkCall() {
		if(!hasLogtalkContext())
			return null;
		Compound logtalkArg = (Compound) getContextTerm();
		Term logtalkCall = logtalkArg.arg(1);
		return (logtalkCall instanceof Var)? null : logtalkCall;
	}
	
	public Term getLogtalkEntity() {
		if(!hasLogtalkContext())
			return null;
		Compound logtalkArg = (Compound) getContextTerm();
		Term logtalkEntity = logtalkArg.arg(2);
		return (logtalkEntity instanceof Var)? null : logtalkEntity;
	}
	
}
