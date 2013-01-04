package org.jpc.salt;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.List;


public abstract class TermWriter<TermType> extends DefaultTermContentHandler {
	/**
	 * The terms that have been written using this TermWritter
	 * Subclasses could decide to stream each processed term somewhere else (e.g., asserting it in a logic engine) instead of storing them here
	 */
	private Deque<TermType> terms = new ArrayDeque<>();
	
	/**
	 * An auxiliar stack for storing partially built terms (like compounds)
	 */
	private Deque<TermBuilder<TermType>> processingStack = new ArrayDeque<>();

	private boolean isProcessingCompound() {
		return !processingStack.isEmpty();
	}
	
	public List<TermType> getTerms() {
		return new ArrayList<>(terms);
	}

	protected void write(TermType term) {
		addLast(term);
	}
	
	protected void addLast(TermType term) {
		terms.addLast(term);
	}
	
//	protected TermType pollLast() {
//		return terms.pollLast();
//	}
	
	protected void process(TermType term) {
		if(!isProcessingCompound())
			write(term);
		else {
			addToProcessingStack(term);
		}
	}
	
	private void addToProcessingStack(TermType term) {
		TermBuilder<TermType> peekCompoundBuilder = processingStack.peek();
		if(!peekCompoundBuilder.hasFunctor())
			peekCompoundBuilder.setFunctor(term);
		else
			peekCompoundBuilder.addArg(term);
	}
	
	@Override
	public TermContentHandler startCompound() {
		processingStack.push(createCompoundBuilder());
		return this;
	}

	@Override
	public TermContentHandler endCompound() {
		process(processingStack.pop().build());
		return this;
	}

	protected abstract TermBuilder<TermType> createCompoundBuilder();
	
}
