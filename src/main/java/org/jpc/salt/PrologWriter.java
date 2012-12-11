package org.jpc.salt;

import org.jpc.JpcException;
import org.jpc.engine.logtalk.LogtalkObject;
import org.jpc.engine.prolog.PrologEngine;
import org.jpc.term.Compound;
import org.jpc.term.Term;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Assert processed terms to the logic database given by the logic engine
 * This class does not store all the processed terms in a list like its parent class
 */
public class PrologWriter extends JpcTermWriter {

	private static Logger logger = LoggerFactory.getLogger(PrologWriter.class);
	
	public PrologEngine logicEngine;
	
	private boolean writeDirective;
	private boolean writePrologClause;
	private boolean writeLogtalkObjectClause;
	private boolean readingLogtalkObjectTerm;
	private Term logtalkObjectTerm;
	
	public PrologWriter(PrologEngine logicEngine) {
		this.logicEngine = logicEngine;
	}

	@Override
	protected void write(Term term) {
		if(readingLogtalkObjectTerm) {
			setLogtalkObjectTerm(term); //the read term is the logtalk object context for the clauses that come next
			readingLogtalkObjectTerm = false;
		} else if(writeDirective) {
			writeDirective(term); //the directive should be executed
		} else if(writePrologClause) {
			writePrologClause(term); //the clause should be asserted
		} else if(writeLogtalkObjectClause) {
			writeLogtalkObjectClause(term); //a logtalk object clause should be asserted in the context of a previously read logtalk object
		} else
			throw new JpcException("A writing mode has not been set");
	}
	
	private void resetWritingMode() {
		writeDirective = false;
		writePrologClause = false;
		writeLogtalkObjectClause = false;
		readingLogtalkObjectTerm = false;
	}
	
	@Override
	public ContentHandler startPrologDirective() {
		resetWritingMode();
		writeDirective = true;
		return this;
	}

	public ContentHandler writeDirective(Term directive) {
		if(!logicEngine.createQuery(directive).hasSolution())
			logger.error("The directive " + directive.toString(logicEngine) + "cannot be executed");
		else
			logger.debug("The directive " + directive.toString(logicEngine) + " has been executed");
		return this;
	}

	@Override
	public ContentHandler startPrologClause() {
		resetWritingMode();
		writePrologClause = true;
		return this;
	}

	public ContentHandler writePrologClause(Term clause) {
		if(!logicEngine.assertz(clause))
			logger.error("Impossible to assert the clause: " + clause.toString(logicEngine) + " in the logic database");
		else
			logger.debug("The clause " + clause.toString(logicEngine) + " has been asserted in the logic database");
		return this;
	}
	
	@Override
	public ContentHandler startLogtalkObjectClause() {
		resetWritingMode();
		writeLogtalkObjectClause = true;
		readingLogtalkObjectTerm = true;
		return this;
	}

	public ContentHandler writeLogtalkObjectClause(Term clause) {
		LogtalkObject logtalkObject = new LogtalkObject(logicEngine, logtalkObjectTerm);
		if(!logtalkObject.assertz(clause))
			logger.error("Impossible to assert the clause: " + clause.toString(logicEngine) + " in the Logtalk object " + logtalkObject.asTerm().toString(logicEngine));
		else
			logger.debug("The clause " + clause.toString(logicEngine) + " has been asserted in the logtalk object" + logtalkObject.asTerm().toString(logicEngine));
		return this;
	}
	
	public void setLogtalkObjectTerm(Term logtalkObjectTerm) {
		this.logtalkObjectTerm = logtalkObjectTerm;
	}
	
}
