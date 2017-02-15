package org.jpc.util.termprocessor;


import java.util.function.Predicate;

import org.jpc.term.Term;

public class ConditionalTermProcessor implements TermProcessor {

    private final TermProcessor termProcessor;
    private final Predicate<Term> predicate;

    public ConditionalTermProcessor (TermProcessor termProcessor, Predicate<Term> predicate) {
        this.termProcessor = termProcessor;
        this.predicate = predicate;
    }

    public void accept(Term term) {
        if (predicate.test(term)) {
            termProcessor.accept(term);
        }
    }

}
