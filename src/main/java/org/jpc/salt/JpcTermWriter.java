package org.jpc.salt;

import org.jpc.term.Atom;
import org.jpc.term.FloatTerm;
import org.jpc.term.IntegerTerm;
import org.jpc.term.Term;
import org.jpc.term.Variable;

public class JpcTermWriter extends TermWriter<Term> {

	@Override
	public void startIntegerTerm(long value) {
		process(new IntegerTerm(value));
	}

	@Override
	public void startFloatTerm(double value) {
		process(new FloatTerm(value));
	}

	@Override
	public void startVariable(String name) {
		process(new Variable(name));
	}
	
	@Override
	public void startAtom(String name) {
		process(new Atom(name));
	}

	protected TermBuilder<Term> createCompoundBuilder() {
		return new JpcTermBuilder();
	}
	
}
