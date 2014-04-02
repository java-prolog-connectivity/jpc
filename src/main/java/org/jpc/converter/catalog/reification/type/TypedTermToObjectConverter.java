package org.jpc.converter.catalog.reification.type;

import java.lang.reflect.Type;

import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.term.Compound;
import org.jpc.term.Term;

public class TypedTermToObjectConverter<T> implements FromTermConverter<Compound, T> {

	public static final String TYPED_TERM_FUNCTOR_NAME = "typed_term";
	
	@Override
	public T fromTerm(Compound term, Type targetType, Jpc jpc) {
		Term sourceTerm = term.arg(1);
		Term targetTypeTerm = term.arg(2);
		Type actualTargetType = jpc.fromTerm(targetTypeTerm, Type.class);
		return jpc.fromTerm(sourceTerm, actualTargetType);
	}

}
