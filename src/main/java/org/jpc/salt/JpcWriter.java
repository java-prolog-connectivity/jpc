package org.jpc.salt;

import java.util.Deque;
import java.util.LinkedList;
import java.util.List;

import org.jpc.salt.helpers.TermBuilder;
import org.jpc.term.Atom;
import org.jpc.term.FloatTerm;
import org.jpc.term.IntegerTerm;
import org.jpc.term.JpcTermBuilder;
import org.jpc.term.Term;
import org.jpc.term.Variable;

public class JpcWriter implements ContentHandler {
	
	private List<Term> terms;
	private Deque<TermBuilder<Term>> processingQueue = new LinkedList<>();

	private boolean isProcessingCompound() {
		return !processingQueue.isEmpty();
	}
	
	public List<Term> terms() {
		return TermBuilder.<Term>asTerms( processingQueue.descendingIterator());
	}
	
	@Override
	public void startIntegerTerm(long value) {
		processingQueue.push(new JpcTermBuilder(new IntegerTerm(value)));
	}

	@Override
	public void startFloatTerm(double value) {
		processingQueue.push(new JpcTermBuilder(new FloatTerm(value)));
	}

	@Override
	public void startVariable(String name) {
		processingQueue.push(new JpcTermBuilder(new Variable(name)));
	}
	
	@Override
	public void startAtom(String name) {
		processingQueue.push(new JpcTermBuilder(new Atom(name)));
	}

	@Override
	public void startCompound() {
		processingQueue.push(new JpcTermBuilder());
	}

	@Override
	public void endCompound() {
	}

	@Override
	public void startCompoundName() {
	}
	
	@Override
	public void endCompoundName() {
		TermBuilder compoundNameBuilder = processingQueue.pop();
		TermBuilder compoundBuilder = processingQueue.peek();
		compoundBuilder.setFunctor(compoundNameBuilder.asTerm());
	}

	@Override
	public void startCompoundArg() {
	}
	
	@Override
	public void endCompoundArg() {
		TermBuilder compoundArgBuilder = processingQueue.pop();
		TermBuilder compoundBuilder = processingQueue.peek();
		compoundBuilder.addArg(compoundArgBuilder.asTerm());
	}

	
}
