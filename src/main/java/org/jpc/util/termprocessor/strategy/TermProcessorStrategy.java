package org.jpc.util.termprocessor.strategy;


import org.jpc.term.Term;
import org.jpc.util.termprocessor.TermProcessor;

public interface TermProcessorStrategy {

    TermProcessor findTermProcessor(Term term);

}
