package org.jpc.typesolver;

import java.lang.reflect.Type;

import org.jpc.term.Atom;
import org.jpc.term.FloatTerm;
import org.jpc.term.IntegerTerm;
import org.jpc.term.Term;
import org.jpc.term.Variable;


public class PrimitiveTypeSolver extends TermTypeSolver {

	@Override
	public Type getType(Term term) {
		if(term instanceof Atom) {
			Atom atom = (Atom) term;
			if(atom.isBoolean())
				return Boolean.class;
			else
				return String.class;
		}
		if(term instanceof IntegerTerm)
			return Long.class;
		if(term instanceof FloatTerm)
			return Double.class;
		if(term instanceof Variable)
			return Object.class; //should be mapped to null
		return null;
	}

}
