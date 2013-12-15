package org.jpc.converter.typesolver.catalog;

import java.lang.reflect.Type;

import org.jpc.converter.typesolver.TypeSolver;
import org.jpc.converter.typesolver.UnrecognizedObjectException;
import org.jpc.term.Atom;
import org.jpc.term.FloatTerm;
import org.jpc.term.IntegerTerm;
import org.jpc.term.Term;


public class ConstantTermTypeSolver implements TypeSolver<Term> {

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
		throw new UnrecognizedObjectException();
	}

}
