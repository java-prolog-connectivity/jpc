package org.jpc.converter;

import java.lang.reflect.Type;

import org.jpc.Jpc;
import org.jpc.term.Term;

public interface FromTermConverter<T extends Term, U> extends JpcConverter {

	U fromTerm(T term, Type targetType, Jpc context);

}
