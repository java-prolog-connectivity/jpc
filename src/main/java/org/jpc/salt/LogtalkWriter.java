package org.jpc.salt;

import org.jpc.JpcException;
import org.jpc.engine.logtalk.LogtalkObject;
import org.jpc.engine.prolog.PrologEngine;
import org.jpc.salt.contenthandler.LogtalkContentHandler;
import org.jpc.term.Compound;
import org.jpc.term.Term;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class LogtalkWriter extends PrologWriter implements LogtalkContentHandler {

	private static Logger logger = LoggerFactory.getLogger(LogtalkWriter.class);
	
	public LogtalkWriter(PrologEngine logicEngine) {
		super(logicEngine);
	}

	@Override
	public void startLogtalkObjectClause() {
	}

	@Override
	public void endLogtalkObjectClause() {
		Term term = pollLast();
		if(!LogtalkObject.isLogtalkMessage(term))
			throw new JpcException("The term " + logicEngine.toString(term) + " is not a logtalk object clause");
		Term logtalkObjectTerm = ((Compound)term).arg(1);
		Term clause = ((Compound)term).arg(2);
		LogtalkObject logtalkObject = new LogtalkObject(logicEngine, logtalkObjectTerm);
		if(!logtalkObject.assertz(clause))
			logger.error("Impossible to assert the clause: " + logicEngine.toString(clause) + " in the Logtalk object " + logicEngine.toString(logtalkObject));
	}

}
