package org.jpc.util.salt;

import java.lang.ref.Reference;

import org.jpc.term.Atom;
import org.jpc.term.Float;
import org.jpc.term.Integer;
import org.jpc.term.JRef.StrongJRef;
import org.jpc.term.JRef.WeakJRef;
import org.jpc.term.Term;
import org.jpc.term.Var;

public class JpcTermStreamer extends TermStreamer<Term> {

	public JpcTermStreamer(TermProcessor<Term> termProcessor) {
		super(termProcessor);
	}

	@Override
	public TermContentHandler startIntegerTerm(long value) {
		process(new Integer(value));
		return this;
	}

	@Override
	public TermContentHandler startFloatTerm(double value) {
		process(new Float(value));
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
