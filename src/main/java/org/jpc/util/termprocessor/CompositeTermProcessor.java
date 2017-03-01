package org.jpc.util.termprocessor;


import java.util.AbstractMap;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.function.Predicate;

import org.jpc.JpcException;
import org.jpc.term.Term;
import org.jpc.util.termprocessor.strategy.PredicateTermProcessorStrategy;
import org.jpc.util.termprocessor.strategy.TermProcessorStrategy;

public class CompositeTermProcessor implements TermProcessor {

    private final TermProcessorStrategy termProcessorStrategy;

    private CompositeTermProcessor(TermProcessorStrategy termProcessorStrategy) {
        this.termProcessorStrategy = termProcessorStrategy;
    }

    @Override
    public void accept(Term term) {
        TermProcessor termProcessor = termProcessorStrategy.findTermProcessor(term);
        termProcessor.accept(term);
    }


    public static Builder builder() {
        return new Builder();
    }


    public static class Builder {

        private final List<Map.Entry<Predicate<Term>, TermProcessor>> entries = new ArrayList<>();

        public CompositeTermProcessor build() {
            return new CompositeTermProcessor(new PredicateTermProcessorStrategy(entries));
        }

        public Builder addProcessor(Predicate<Term> predicate, TermProcessor termProcessor) {
            entries.add(new AbstractMap.SimpleEntry<>(predicate, termProcessor));
            return this;
        }
    }

    public static class NoTermProcessorAvailableException extends JpcException {

        private final Term term;

        public NoTermProcessorAvailableException(Term term) {
            this.term = term;
        }

        @Override
        public String getMessage() {
            return "No term processor defined for: " + term;
        }
    }

}
