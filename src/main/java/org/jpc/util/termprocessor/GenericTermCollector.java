package org.jpc.util.termprocessor;


import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.List;

public class GenericTermCollector<TermType> implements GenericTermProcessor<TermType> {

    private Deque<TermType> terms = new ArrayDeque<>();

    public List<TermType> getTerms() {
        return new ArrayList<>(terms);
    }

    @Override
    public void process(TermType term) {
        terms.add(term);
    }

    public TermType getFirst() {
        return getTerms().get(0);
    }
}
