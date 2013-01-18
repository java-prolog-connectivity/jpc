package org.jpc.converter.toterm;

import org.jpc.term.Term;

import com.google.common.base.Function;

public interface  ToTermConverter<O> extends Function<O, Term>{

}
