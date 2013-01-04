package org.jpc.converter.toterm;

import org.jpc.term.Term;

import com.google.common.base.Function;

public interface  ObjectToTermConverter<O> extends Function<O, Term>{

}
