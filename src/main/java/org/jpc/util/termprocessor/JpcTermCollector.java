package org.jpc.util.termprocessor;


import org.jpc.term.Term;

public class JpcTermCollector extends GenericTermCollector<Term> implements TermProcessor {

    public static JpcTermCollector termCollector() {
        return new JpcTermCollector();
    }

}
