package org.jpc.util.termprocessor;


import org.jpc.term.Term;

public class TermCollector extends GenericTermCollector<Term> implements TermProcessor {

    public static TermCollector termCollector() {
        return new TermCollector();
    }

}
