package org.jpc.engine.embedded.indexing;

import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.Term;

import com.google.common.base.Function;

public class CompoundFunctorIndexFunction implements Function<Term, Object> {

	@Override
	public Object apply(Term term) {
		Object key = null;
		if(term instanceof Compound) {
			Compound compound = (Compound)term;
			Term name = compound.getName();
			if(name.isGround()) //the name of the compound should be ground in order to be indexable.
				key = compound.getFunctor();
		} else if(term instanceof Atom) {
			return key = term;
		} 
		if(key == null)
			throw new NonIndexableTermException(term, this);
		return key;
	}
	
}
