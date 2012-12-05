package org.jpc.util.salt;

import org.jpc.engine.prolog.PrologEngine;
import org.jpc.salt.JpcTermWriter;
import org.jpc.term.Term;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;



public class LogicEngineWriter extends JpcTermWriter {

	private static Logger logger = LoggerFactory.getLogger(LogicEngineWriter.class);
	
	public PrologEngine logicEngine;
	
	public LogicEngineWriter(PrologEngine logicEngine) {
		this.logicEngine = logicEngine;
	}
	
	/**
	 * Assert processed terms to the logic database given by the logic engine
	 * This class does not store all the processed terms in a list like its parent class (then it does not need to make a super call in this method)
	 */
	@Override
	public void addTerm(Term term) {
		if(!logicEngine.assertz(term)) {
			logger.error("Impossible to assert the term: " + term + " in the logic database");
		}
	}
	
}
