package org.jpc.converter.fromterm;

import org.jpc.term.Term;

import com.google.common.base.Function;

public interface TermToObjectConverter<T> extends Function<Term, T> {

}
