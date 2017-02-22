package org.jpc.util.termprocessor;


import static java.util.Arrays.asList;
import static org.jpc.term.Compound.compound;
import static org.jpc.term.JRef.jRef;
import static org.jpc.term.Var.var;

import java.util.Optional;

import org.jpc.JpcException;
import org.jpc.engine.embedded.JpcEngine;
import org.jpc.engine.prolog.PrologEngine;
import org.jpc.query.Query;
import org.jpc.query.Solution;
import org.jpc.term.Functor;
import org.jpc.term.Term;
import org.jpc.term.Var;

public class SwitchTermProcessor implements TermProcessor {

    private static final String TERM_PROCESSOR_FUNCTOR = "term_processor";

    private final PrologEngine prologEngine;

    private SwitchTermProcessor(PrologEngine prologEngine) {
        this.prologEngine = prologEngine;
    }

    @Override
    public void accept(Term term) {
        TermProcessor termProcessor = findProcessorOrThrow(term);
        termProcessor.accept(term);
    }

    private TermProcessor findProcessorOrThrow(Term term) {
        TermProcessor termProcessor = null;
        Var var = var("Processor");
        Query query = prologEngine.query(compound(TERM_PROCESSOR_FUNCTOR, asList(term, var)));
        Optional<Solution> solution = query.oneSolution();
        if (solution.isPresent()) {
            return solution.get().asObject(var);
        } else {
            throw new JpcException("No processor defined for term: " + term);
        }
    }

    public static Builder builder() {
        return new Builder();
    }

    public static class Builder {

        private final PrologEngine prologEngine = new JpcEngine();

        public SwitchTermProcessor build() {
            return new SwitchTermProcessor(prologEngine);
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
