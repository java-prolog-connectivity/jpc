package org.jpc.salt;

import org.jpc.engine.logtalk.LogtalkObject;
import org.jpc.engine.prolog.PrologEngine;
import org.jpc.term.Term;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Assert processed terms to the logic database given by a logic engine
 */
public class LogicEngineWriter extends PrologWriter {

	private static Logger logger = LoggerFactory.getLogger(LogicEngineWriter.class);

	public LogicEngineWriter(PrologEngine logicEngine) {
		super(logicEngine);
	}

	public void writePrologDirective(Term directive) {
		if(!getLogicEngine().query(directive).hasSolution())
			logger.error("The directive: " + directive.toString(getLogicEngine()) + "cannot be executed");
		else
			logger.debug("The directive: " + directive.toString(getLogicEngine()) + " has been executed");
	}
	
	public void writeLogtalkObjectDirective(Term directive) {
		LogtalkObject logtalkObject = new LogtalkObject(getCurrentLogtalkObjectTerm(), getLogicEngine());
		if(!logtalkObject.perform(directive).hasSolution())
			logger.error("The logtalk object: " + logtalkObject.asTerm().toString(getLogicEngine()) + " cannot execute the directive: " + directive.toString(getLogicEngine()));
		else
			logger.debug("The logtalk object: " + logtalkObject.asTerm().toString(getLogicEngine()) + " has executed the directive: " + directive.toString(getLogicEngine()));
	}
	
	public void writePrologClause(Term clause) {
		if(!getLogicEngine().assertz(clause))
			logger.error("Impossible to assert the clause: " + clause.toString(getLogicEngine()) + " in the logic database");
		else
			logger.debug("The clause: " + clause.toString(getLogicEngine()) + " has been asserted in the logic database");
	}
	
	public void writeLogtalkObjectClause(Term clause) {
		try {
			LogtalkObject logtalkObject = new LogtalkObject(getCurrentLogtalkObjectTerm(), getLogicEngine());
			if(!logtalkObject.assertz(clause))
				logger.error("Impossible to assert the clause: " + clause.toString(getLogicEngine()) + " in the logtalk object: " + logtalkObject.asTerm().toString(getLogicEngine()));
			else
				logger.debug("The clause: " + clause.toString(getLogicEngine()) + " has been asserted in the logtalk object: " + logtalkObject.asTerm().toString(getLogicEngine()));
		} catch(Exception e) {
			//System.out.println(e);
		}
		
	}
	
	
}
