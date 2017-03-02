package org.jpc.term;


import static java.util.Arrays.asList;
import static org.jpc.term.Atom.atom;
import static org.jpc.term.Compound.compound;

import java.util.Objects;

import org.jpc.engine.prolog.PrologConstants;

public abstract class TermConstants {

    public static final Atom TRUE = atom(PrologConstants.TRUE);
    public static final Atom FAIL = atom(PrologConstants.FAIL);
    public static final Atom FALSE = atom(PrologConstants.FALSE);
    public static final Atom NIL = atom(PrologConstants.NIL_SYMBOL);
    public static final Term JAVA_NULL = compound("@", asList(atom(Objects.toString(null))));


}
