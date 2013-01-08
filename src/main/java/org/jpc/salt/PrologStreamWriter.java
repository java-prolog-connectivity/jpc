package org.jpc.salt;

import static java.util.Arrays.asList;
import static org.jpc.engine.logtalk.LogtalkObject.logtalkMessage;
import static org.jpc.engine.prolog.PrologConstants.ASSERTZ;

import java.io.PrintStream;

import org.jpc.engine.logtalk.LogtalkObject;
import org.jpc.engine.prolog.PrologEngine;
import org.jpc.term.Compound;
import org.jpc.term.Term;

/**
 * Writes the text representation of the logic terms to a given output stream
 */
public class PrologStreamWriter extends PrologWriter {

	private PrintStream out;
	public static final String DIRECTIVE_PREFIX = ":- ";
	
	
	public PrologStreamWriter(PrologEngine logicEngine, PrintStream out) {
		super(logicEngine);
		this.out = out;
	}

	@Override
	public void writePrologDirective(Term directive) {
		String termString = directive.toString(getLogicEngine());
		out.println(DIRECTIVE_PREFIX + termString+".");
		
	}

	@Override
	public void writeLogtalkObjectDirective(Term directive) {
		String termString = logtalkMessage(getCurrentLogtalkObjectTerm(), directive).toString(getLogicEngine());
		out.println(DIRECTIVE_PREFIX + termString+".");
	}

	@Override
	public void writePrologClause(Term clause) {
		String termString = clause.toString(getLogicEngine());
		out.println(termString+".");
	}

	@Override
	public void writeLogtalkObjectClause(Term clause) {
		Term assertTerm = new Compound(ASSERTZ, asList(clause));
		String termString = logtalkMessage(getCurrentLogtalkObjectTerm(), assertTerm).toString(getLogicEngine());
		out.println(DIRECTIVE_PREFIX + termString+".");
	}

}
