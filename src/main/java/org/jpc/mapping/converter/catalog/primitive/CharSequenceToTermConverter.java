package org.jpc.mapping.converter.catalog.primitive;

import org.jpc.Jpc;
import org.jpc.mapping.converter.SimpleToTermConverter;
import org.jpc.term.Atom;
import org.jpc.term.Term;

public class CharSequenceToTermConverter implements SimpleToTermConverter<CharSequence> {

    @Override
    public Term toTerm(final CharSequence object, final Jpc context) {
        return new Atom(object.toString());
    }
}
