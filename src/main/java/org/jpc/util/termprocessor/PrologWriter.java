package org.jpc.util.termprocessor;

import org.jpc.JpcException;
import org.jpc.engine.dialect.Dialect;
import org.jpc.term.Term;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public abstract class PrologWriter implements TermProcessor {

	private static final Logger logger = LoggerFactory.getLogger(PrologWriter.class);

	private final Dialect dialect;

	private boolean writeDirective;
	private boolean writeClause;
	private boolean readingLogtalkObjectTerm;
	private Term currentLogtalkObjectTerm;


	public PrologWriter() {
		this(Dialect.JPC);
	}

	public PrologWriter(Dialect dialect) {
		this.dialect = dialect;
	}

	public Dialect getDialect() {
		return dialect;
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
	public void accept(Term term) {
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

	//next read terms will be interpreted as goals to be executed
	public void followingDirectives() {
		resetWritingMode();
		writeDirective = true;
	}

	//next read terms will be considered dynamic clauses to be asserted
	public void followingDynamicClauses() {
		resetWritingMode();
		writeClause = true;
	}

	public void startLogtalkObjectContext() {
		readingLogtalkObjectTerm = true; //the next term to come will be considered the logtalk object context
	}

	public void endLogtalkObjectContext() {
		readingLogtalkObjectTerm = false;
		resetLogtalkOjectTerm();
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
