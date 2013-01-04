package org.jpc.salt;

import org.jpc.JpcException;
import org.jpc.engine.logtalk.LogtalkObject;
import org.jpc.engine.prolog.PrologEngine;
import org.jpc.term.Term;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Assert processed terms to the logic database given by the logic engine
 * This class does not store all the processed terms in a list like its parent class
 */
public class PrologWriter extends JpcTermWriter implements PrologContentHandler {

	private static Logger logger = LoggerFactory.getLogger(PrologWriter.class);
	
	public PrologEngine logicEngine;
	
	private boolean writeDirective;
	private boolean writeClause;
	private boolean readingLogtalkObjectTerm;
	private Term logtalkObjectTerm;
	
	public PrologWriter(PrologEngine logicEngine) {
		this.logicEngine = logicEngine;
	}

	@Override
	protected void write(Term term) {
		if(readingLogtalkObjectTerm) {
			setLogtalkObjectTerm(term); //the read term is the logtalk object context for the instructions that come next
			readingLogtalkObjectTerm = false;
		} else if(writeDirective) {
			writeDirective(term); //the directive should be executed
		} else if(writeClause) {
			writeClause(term); //the clause should be asserted
		} else
			throw new JpcException("A writing mode has not been set");
	}
	
	private void resetWritingMode() {
		writeDirective = false;
		writeClause = false;
		readingLogtalkObjectTerm = false;
	}
	
	@Override
	public TermContentHandler comingDirectives() {
		resetWritingMode();
		writeDirective = true;
		return this;
	}

	@Override
	public TermContentHandler comingDynamicClauses() {
		resetWritingMode();
		writeClause = true;
		return this;
	}

	@Override
	public TermContentHandler startLogtalkObjectContext() {
		readingLogtalkObjectTerm = true; //the next term to come will be considered the logtalk object context
		return this;
	}

	@Override
	public TermContentHandler endLogtalkObjectContext() {
		readingLogtalkObjectTerm = false;
		resetLogtalkOjectTerm();
		return this;
	}
	
	public void writeDirective(Term directive) {
		if(logtalkObjectTerm == null)
			writePrologDirective(directive);
		else
			writeLogtalkObjectDirective(directive);
	}
	
	public void writePrologDirective(Term directive) {
		if(!logicEngine.createQuery(directive).hasSolution())
			logger.error("The directive: " + directive.toString(logicEngine) + "cannot be executed");
		else
			logger.debug("The directive: " + directive.toString(logicEngine) + " has been executed");
	}
	
	public void writeLogtalkObjectDirective(Term directive) {
		LogtalkObject logtalkObject = new LogtalkObject(logicEngine, logtalkObjectTerm);
		if(!logtalkObject.perform(directive).hasSolution())
			logger.error("The logtalk object: " + logtalkObject.asTerm().toString(logicEngine) + " cannot execute the directive: " + directive.toString(logicEngine));
		else
			logger.debug("The logtalk object: " + logtalkObject.asTerm().toString(logicEngine) + " has executed the directive: " + directive.toString(logicEngine));
	}
	
	public void writeClause(Term clause) {
		if(logtalkObjectTerm == null)
			writePrologClause(clause);
		else
			writeLogtalkObjectClause(clause);
	}
	
	public void writePrologClause(Term clause) {
		if(!logicEngine.assertz(clause))
			logger.error("Impossible to assert the clause: " + clause.toString(logicEngine) + " in the logic database");
		else
			logger.debug("The clause: " + clause.toString(logicEngine) + " has been asserted in the logic database");
	}
	
	public void writeLogtalkObjectClause(Term clause) {
		try {
			LogtalkObject logtalkObject = new LogtalkObject(logicEngine, logtalkObjectTerm);
			if(!logtalkObject.assertz(clause))
				logger.error("Impossible to assert the clause: " + clause.toString(logicEngine) + " in the logtalk object: " + logtalkObject.asTerm().toString(logicEngine));
			else
				logger.debug("The clause: " + clause.toString(logicEngine) + " has been asserted in the logtalk object: " + logtalkObject.asTerm().toString(logicEngine));
		} catch(Exception e) {
			//System.out.println(e);
		}
		
	}
	
	private void resetLogtalkOjectTerm() {
		logtalkObjectTerm = null;
	}
	
	private void setLogtalkObjectTerm(Term logtalkObjectTerm) {
		this.logtalkObjectTerm = logtalkObjectTerm;
	}

}

