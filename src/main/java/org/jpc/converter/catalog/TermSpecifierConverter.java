package org.jpc.converter.catalog;

import java.lang.reflect.Type;

import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.term.Compound;
import org.jpc.term.Term;

public class TermSpecifierConverter<T extends Term> implements FromTermConverter<Compound, T>{

	public static final String TERM_SPECIFIER_FUNCTOR_NAME = "term";

	@Override
	public T fromTerm(Compound term, Type targetType, Jpc jpc) {
		return (T) term.arg(1);
	}
	
}
