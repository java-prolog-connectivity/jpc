package org.jpc.util.termprocessor.strategy;


import java.util.List;
import java.util.Map;
import java.util.function.Predicate;

import org.jpc.term.Term;
import org.jpc.util.termprocessor.CompositeTermProcessor;
import org.jpc.util.termprocessor.TermProcessor;

public class PredicateTermProcessorStrategy implements TermProcessorStrategy {

    public final List<Map.Entry<Predicate<Term>, TermProcessor>> entries;

    public PredicateTermProcessorStrategy(List<Map.Entry<Predicate<Term>, TermProcessor>> entries) {
        this.entries = entries;
    }

    @Override
    public TermProcessor findTermProcessor(Term term) {
        Term compiledTerm = term;
        //Term compiledTerm = term.compile(); //currently needed to test for unification
        return entries.stream().filter(entry -> entry.getKey().test(compiledTerm)).map(Map.Entry::getValue).findFirst().orElseGet(
                () -> {throw new CompositeTermProcessor.NoTermProcessorAvailableException(compiledTerm);});
    }

}
