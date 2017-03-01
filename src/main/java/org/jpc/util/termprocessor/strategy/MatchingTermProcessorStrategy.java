package org.jpc.util.termprocessor.strategy;


import static java.util.Arrays.asList;
import static org.jpc.term.Compound.compound;
import static org.jpc.term.JRef.jRef;
import static org.jpc.term.Var.var;

import java.util.Optional;

import org.jpc.engine.embedded.JpcEngine;
import org.jpc.engine.prolog.PrologEngine;
import org.jpc.query.Query;
import org.jpc.query.Solution;
import org.jpc.term.Functor;
import org.jpc.term.Term;
import org.jpc.term.Var;
import org.jpc.util.termprocessor.CompositeTermProcessor.NoTermProcessorAvailableException;
import org.jpc.util.termprocessor.TermProcessor;

import com.google.common.annotations.Beta;

/**
 * Alternative implementation to PredicateTermProcessorStrategy.
 * May be deleted soon.
 */
@Beta
public class MatchingTermProcessorStrategy implements TermProcessorStrategy {

    private static final String TERM_PROCESSOR_FUNCTOR = "term_processor";

    private final PrologEngine prologEngine;

    private MatchingTermProcessorStrategy(PrologEngine prologEngine) {
        this.prologEngine = prologEngine;
    }

    @Override
    public TermProcessor findTermProcessor(Term term) {
        TermProcessor termProcessor = null;
        Var var = var("Processor");
        Query query = prologEngine.query(compound(TERM_PROCESSOR_FUNCTOR, asList(term, var)));
        Optional<Solution> solution = query.oneSolution();
        if (solution.isPresent()) {
            return solution.get().asObject(var);
        } else {
            throw new NoTermProcessorAvailableException(term);
        }
    }

    public static Builder builder() {
        return new Builder();
    }


    public static class Builder {

        private final PrologEngine prologEngine = new JpcEngine();

        public MatchingTermProcessorStrategy build() {
            return new MatchingTermProcessorStrategy(prologEngine);
        }

        public Builder addProcessor(Term term, TermProcessor termProcessor) {
            prologEngine.assertz(compound(TERM_PROCESSOR_FUNCTOR, asList(term, jRef(termProcessor))));
            return this;
        }

        public Builder addProcessor(Functor functor, TermProcessor termProcessor) {
            return addProcessor(functor.asTerm(), termProcessor);
        }

    }

}