package org.jpc.util.termprocessor;

import static java.util.Arrays.asList;
import static org.jpc.engine.logtalk.LogtalkObject.logtalkMessage;
import static org.jpc.engine.prolog.PrologConstants.ASSERTZ;

import java.io.PrintStream;

import org.jpc.engine.dialect.Dialect;
import org.jpc.engine.prolog.OperatorsContext;
import org.jpc.term.Compound;
import org.jpc.term.Term;

/**
 * Writes the text representation of the logic terms to a given output stream
 */
public class PrologPrintStreamWriter extends PrologWriter {

	public static final String DIRECTIVE_PREFIX = ":- ";

	protected final OperatorsContext operatorsContext;
	protected final PrintStream out;


	public PrologPrintStreamWriter(Dialect dialect, OperatorsContext operatorsContext, PrintStream out) {
		super(dialect);
		this.operatorsContext = operatorsContext;
		this.out = out;
	}
	
	@Override
	public void writePrologDirective(Term directive) {
		String termString = directive.toEscapedString(getDialect(), operatorsContext);
		out.println(DIRECTIVE_PREFIX + termString+".");
	}

	@Override
	public void writeLogtalkObjectDirective(Term directive) {
		String termString = logtalkMessage(getCurrentLogtalkObjectTerm(), directive)
				.toEscapedString(getDialect(), operatorsContext);
		out.println(DIRECTIVE_PREFIX + termString+".");
	}

	@Override
	public void writePrologClause(Term clause) {
		String termString = clause.toEscapedString(getDialect(), operatorsContext);
		out.println(termString+".");
	}

	@Override
	public void writeLogtalkObjectClause(Term clause) {
		Term assertTerm = new Compound(ASSERTZ, asList(clause));
		String termString = logtalkMessage(getCurrentLogtalkObjectTerm(), assertTerm)
				.toEscapedString(getDialect(), operatorsContext);
		out.println(DIRECTIVE_PREFIX + termString+".");
	}

}
