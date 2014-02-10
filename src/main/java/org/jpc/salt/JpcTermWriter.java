package org.jpc.salt;

import org.jpc.term.Atom;
import org.jpc.term.FloatTerm;
import org.jpc.term.IntegerTerm;
import org.jpc.term.JRef;
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
		process(JRef.jref(ref));
		return this;
	}
	
	@Override
	public TermContentHandler startSoftJRef(Object ref) {
		process(JRef.softJref(ref));
		return this;
	}
	
	@Override
	public TermContentHandler startWeakJRef(Object ref) {
		process(JRef.weakJref(ref));
		return this;
	}
	
	protected TermBuilder<Term> createCompoundBuilder() {
		return new JpcTermBuilder();
	}
	
}
