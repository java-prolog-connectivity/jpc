package org.jpc.salt;

import org.jpc.engine.prolog.PrologEngine;
import org.jpc.salt.contenthandler.PrologContentHandler;
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
	
	public PrologWriter(PrologEngine logicEngine) {
		this.logicEngine = logicEngine;
	}

	@Override
	public void startPrologDirective() {
	}

	@Override
	public void endPrologDirective() {
		Term directive = pollLast();
		if(!logicEngine.createQuery(directive).hasSolution())
			logger.error("The directive " + logicEngine.toString(directive) + "cannot be executed");
	}

	@Override
	public void startPrologClause() {
	}

	@Override
	public void endPrologClause() {
		Term clause = pollLast();
		if(!logicEngine.assertz(clause))
			logger.error("Impossible to assert the clause: " + logicEngine.toString(clause) + " in the logic database");
	}
	
}
