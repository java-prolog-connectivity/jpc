package org.jpc.util.salt;


import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.List;

public class TermCollector<TermType> implements TermProcessor<TermType> {

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
