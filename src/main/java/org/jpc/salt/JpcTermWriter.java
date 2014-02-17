package org.jpc.salt;

import java.lang.ref.Reference;

import org.jpc.term.Atom;
import org.jpc.term.FloatTerm;
import org.jpc.term.IntegerTerm;
import org.jpc.term.JRef.StrongJRef;
import org.jpc.term.JRef.WeakJRef;
import org.jpc.term.Term;
import org.jpc.term.Var;

public class JpcTermWriter extends TermWriter<Term> {

	@Override
	public TermContentHandler startIntegerTerm(long value) {
		process(new IntegerTerm(value));
		return this;
	}

	@Override
	public TermContentHandler startFloatTerm(double value) {
		process(new FloatTerm(value));
		return this;
	}

	@Override
	public TermContentHandler startVariable(String name) {
		process(new Var(name));
		return this;
	}
	
	@Override
	public TermContentHandler startAtom(String name) {
		process(new Atom(name));
		return this;
	}

	@Override
	public TermContentHandler startJRef(Object ref) {
		if(ref instanceof Reference)
			process(new WeakJRef((Reference)ref));
		else
			process(new StrongJRef(ref));
		return this;
	}
	
	protected TermBuilder<Term> createCompoundBuilder() {
		return new JpcTermBuilder();
	}
	
}
