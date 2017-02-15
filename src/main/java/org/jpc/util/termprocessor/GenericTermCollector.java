package org.jpc.util.termprocessor;


import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.List;
import java.util.function.Consumer;

/**
 * This class implements Consumer<TermType> instead of TermProcessor since it is used by JPC drivers.
 * Since those drivers deal with arbitrary term types, a generic class was preferred here.
 *
 * @param <TermType> the term type.
 */
public class GenericTermCollector<TermType> implements Consumer<TermType> {

    private Deque<TermType> terms = new ArrayDeque<>();

    public List<TermType> getTerms() {
        return new ArrayList<>(terms);
    }

    @Override
    public void accept(TermType term) {
        terms.add(term);
    }

    public TermType getFirst() {
        return getTerms().get(0);
    }
}
