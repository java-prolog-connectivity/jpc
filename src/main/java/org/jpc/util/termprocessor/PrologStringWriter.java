package org.jpc.util.termprocessor;


import static java.util.Arrays.asList;
import static org.jpc.engine.logtalk.LogtalkObject.logtalkMessage;
import static org.jpc.engine.prolog.PrologConstants.ASSERTZ;

import org.jpc.engine.dialect.Dialect;
import org.jpc.engine.prolog.OperatorsContext;
import org.jpc.term.Compound;
import org.jpc.term.Term;

public abstract class PrologStringWriter extends PrologAbstractWriter {

    public static final String DIRECTIVE_PREFIX = ":- ";

    private final OperatorsContext operatorsContext;


    protected PrologStringWriter(Dialect dialect, OperatorsContext operatorsContext) {
        super(dialect);
        this.operatorsContext = operatorsContext;
    }

    protected abstract void println(String string);

    public OperatorsContext getOperatorsContext() {
        return operatorsContext;
    }

    @Override
    public void writePrologDirective(Term directive) {
        String termString = directive.toEscapedString(getDialect(), getOperatorsContext());
        println(DIRECTIVE_PREFIX + termString+".");
    }

    @Override
    public void writeLogtalkObjectDirective(Term directive) {
        String termString = logtalkMessage(getCurrentLogtalkObjectTerm(), directive)
                .toEscapedString(getDialect(), getOperatorsContext());
        println(DIRECTIVE_PREFIX + termString+".");
    }

    @Override
    public void writePrologClause(Term clause) {
        String termString = clause.toEscapedString(getDialect(), getOperatorsContext());
        println(termString+".");
    }

    @Override
    public void writeLogtalkObjectClause(Term clause) {
        Term assertTerm = new Compound(ASSERTZ, asList(clause));
        String termString = logtalkMessage(getCurrentLogtalkObjectTerm(), assertTerm)
                .toEscapedString(getDialect(), getOperatorsContext());
        println(DIRECTIVE_PREFIX + termString+".");
    }

}
