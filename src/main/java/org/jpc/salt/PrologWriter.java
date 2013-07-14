package org.jpc.salt;

import org.jpc.JpcException;
import org.jpc.engine.prolog.PrologEngine;
import org.jpc.term.Term;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Write logic terms to a "target". 
 * This target is defined by subclasses. For example, it may be a logic engine or just a text file.
 */
public abstract class PrologWriter extends JpcTermWriter implements PrologContentHandler {

	private static Logger logger = LoggerFactory.getLogger(PrologWriter.class);

	private boolean writeDirective;
	private boolean writeClause;
	private boolean readingLogtalkObjectTerm;
	private Term currentLogtalkObjectTerm;
	
	public PrologWriter() {
	}

	private void resetLogtalkOjectTerm() {
		currentLogtalkObjectTerm = null;
	}
	
	private void setCurrentLogtalkObjectTerm(Term logtalkObjectTerm) {
		this.currentLogtalkObjectTerm = logtalkObjectTerm;
	}

	public Term getCurrentLogtalkObjectTerm() {
		return currentLogtalkObjectTerm;
	}
	
	
	@Override
	protected void write(Term term) {
		if(readingLogtalkObjectTerm) {
			setCurrentLogtalkObjectTerm(term); //the read term is the logtalk object context for the instructions that come next
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
	public TermContentHandler followingDirectives() {
		resetWritingMode();
		writeDirective = true;
		return this;
	}

	@Override
	public TermContentHandler followingDynamicClauses() {
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
		if(getCurrentLogtalkObjectTerm() == null)
			writePrologDirective(directive);
		else
			writeLogtalkObjectDirective(directive);
	}
	
	public abstract void writePrologDirective(Term directive);
	
	public abstract void writeLogtalkObjectDirective(Term directive);
	
	public void writeClause(Term clause) {
		if(getCurrentLogtalkObjectTerm() == null)
			writePrologClause(clause);
		else
			writeLogtalkObjectClause(clause);
	}
	
	public abstract void writePrologClause(Term clause);
	
	public abstract void writeLogtalkObjectClause(Term clause);

}


