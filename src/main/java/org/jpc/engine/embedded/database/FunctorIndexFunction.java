package org.jpc.engine.embedded.database;

import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.Term;

import java.util.function.Function;

/**
 * Index function mapping a term to its functor name.
 * @author sergioc
 *
 */
public class FunctorIndexFunction implements Function<Term, Object> {

	@Override
	public Object apply(Term term) {
		Object key = null;
		if(term instanceof Compound) {
			Compound compound = (Compound)term;
			Term name = compound.getNameTerm();
			if(name.isGround()) //the name of the compound should be ground in order to be indexable.
				key = compound.getFunctor();
		} else if(term instanceof Atom) {
			return key = ((Atom)term).getName();
		} 
		if(key == null)
			throw new NonIndexableTermException(term, this);
		return key;
	}
	
}
