package org.jpc.util;

import java.util.List;

import org.jpc.term.Compound;
import org.jpc.term.Term;

public class JpcTermBuilder extends TermBuilder<Term> {//implements TermAdaptable {

	public JpcTermBuilder() {
	}
	
	public JpcTermBuilder(Term functor) {
		super(functor);
	}
	
	public JpcTermBuilder(Term functor, List<Term> args) {
		super(functor, args);
	}

	@Override
	public Term asTerm() {
		Term builtTerm;
		Term termFunctor = (Term) getFunctor();
		if(arity() == 0)
			builtTerm = termFunctor;
		else
			builtTerm = new Compound(termFunctor, getArgs());
		return builtTerm;
	}

}
