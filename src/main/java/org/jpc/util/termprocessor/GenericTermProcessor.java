package org.jpc.util.termprocessor;

/**
 * Direct instances of this class are only used for building JPC drivers
 * since it allows to work with terms different than the ones provided by JPC.
 * For any other use case prefer its subclass TermProcessor.
 * @see TermProcessor
 * @param <TermType> the type of the term.
 */
public interface GenericTermProcessor<TermType> {

    void process(TermType tem);

}
