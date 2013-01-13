package org.jpc.salt;

import org.jpc.engine.logtalk.LogtalkObject;
import org.jpc.engine.prolog.PrologEngine;
import org.jpc.term.Term;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Assert processed terms to the logic database given by a logic engine
 */
public class PrologEngineWriter extends PrologWriter {

	private static Logger logger = LoggerFactory.getLogger(PrologEngineWriter.class);

	public PrologEngineWriter(PrologEngine prologEngine) {
		super(prologEngine);
	}

	public void writePrologDirective(Term directive) {
		if(!getPrologEngine().query(directive).hasSolution())
			logger.error("The directive: " + directive.toString(getPrologEngine()) + "cannot be executed");
		else
			logger.trace("The directive: " + directive.toString(getPrologEngine()) + " has been executed");
	}
	
	public void writeLogtalkObjectDirective(Term directive) {
		LogtalkObject logtalkObject = new LogtalkObject(getCurrentLogtalkObjectTerm(), getPrologEngine());
		if(!logtalkObject.perform(directive).hasSolution())
			logger.error("The logtalk object: " + logtalkObject.asTerm().toString(getPrologEngine()) + " cannot execute the directive: " + directive.toString(getPrologEngine()));
		else
			logger.trace("The logtalk object: " + logtalkObject.asTerm().toString(getPrologEngine()) + " has executed the directive: " + directive.toString(getPrologEngine()));
	}
	
	public void writePrologClause(Term clause) {
		if(!getPrologEngine().assertz(clause))
			logger.error("Impossible to assert the clause: " + clause.toString(getPrologEngine()) + " in the logic database");
		else
			logger.trace("The clause: " + clause.toString(getPrologEngine()) + " has been asserted in the logic database");
	}
	
	public void writeLogtalkObjectClause(Term clause) {
		try {
			LogtalkObject logtalkObject = new LogtalkObject(getCurrentLogtalkObjectTerm(), getPrologEngine());
			if(!logtalkObject.assertz(clause))
				logger.error("Impossible to assert the clause: " + clause.toString(getPrologEngine()) + " in the logtalk object: " + logtalkObject.asTerm().toString(getPrologEngine()));
			else
				logger.trace("The clause: " + clause.toString(getPrologEngine()) + " has been asserted in the logtalk object: " + logtalkObject.asTerm().toString(getPrologEngine()));
		} catch(Exception e) {
			//System.out.println(e);
		}
		
	}
	
	
}
