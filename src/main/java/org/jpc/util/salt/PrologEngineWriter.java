package org.jpc.util.salt;

import org.jpc.engine.logtalk.LogtalkObject;
import org.jpc.engine.prolog.PrologEngine;
import org.jpc.term.Term;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Assert processed terms to the logic database given by a logic engine
 */
public class PrologEngineWriter extends PrologWriter {

	private static final Logger logger = LoggerFactory.getLogger(PrologEngineWriter.class);

	private PrologEngine prologEngine;
	
	public PrologEngineWriter(PrologEngine prologEngine) {
		this.prologEngine = prologEngine;
	}

	public PrologEngine getPrologEngine() {
		return prologEngine;
	}
	
	public void writePrologDirective(Term directive) {
		if(!getPrologEngine().query(directive).hasSolution())
			logger.error("The directive: " + directive.toEscapedString() + "cannot be executed");
		else
			logger.trace("The directive: " + directive.toEscapedString() + " has been executed");
	}
	
	public void writeLogtalkObjectDirective(Term directive) {
		LogtalkObject logtalkObject = new LogtalkObject(getCurrentLogtalkObjectTerm(), getPrologEngine());
		if(!logtalkObject.perform(directive).hasSolution())
			logger.error("The logtalk object: " + logtalkObject.asTerm().toEscapedString() + " cannot execute the directive: " + directive.toEscapedString());
		else
			logger.trace("The logtalk object: " + logtalkObject.asTerm().toEscapedString() + " has executed the directive: " + directive.toEscapedString());
	}
	
	public void writePrologClause(Term clause) {
		if(!getPrologEngine().assertz(clause))
			logger.error("Impossible to assert the clause: " + clause.toEscapedString() + " in the logic database");
		else
			logger.trace("The clause: " + clause.toEscapedString() + " has been asserted in the logic database");
	}
	
	public void writeLogtalkObjectClause(Term clause) {
		try {
			LogtalkObject logtalkObject = new LogtalkObject(getCurrentLogtalkObjectTerm(), getPrologEngine());
			if(!logtalkObject.assertz(clause))
				logger.error("Impossible to assert the clause: " + clause.toEscapedString() + " in the logtalk object: " + logtalkObject.asTerm().toEscapedString());
			else
				logger.trace("The clause: " + clause.toEscapedString() + " has been asserted in the logtalk object: " + logtalkObject.asTerm().toEscapedString());
		} catch(Exception e) {
			//System.out.println(e);
		}
		
	}
	
	
}
