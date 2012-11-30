package org.jpc.salt;

import java.util.List;

import org.jpc.term.Compound;
import org.jpc.term.Term;

/**
 * A utility class for creating JPC term objects incrementally
 * @author sergioc
 *
 */
public class JpcTermBuilder extends TermBuilder<Term> {

	public JpcTermBuilder() {
	}
	
	public JpcTermBuilder(Term functor) {
		super(functor);
	}
	
	public JpcTermBuilder(Term functor, List<Term> args) {
		super(functor, args);
	}

	@Override
	public Term build() {
		Term builtTerm;
		if(!isCompound())
			builtTerm = getFunctor();
		else
			builtTerm = new Compound(getFunctor(), getArgs());
		return builtTerm;
	}

}
