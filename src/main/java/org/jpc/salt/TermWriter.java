package org.jpc.salt;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.List;

import org.jpc.salt.contenthandler.DefaultTermContentHandler;

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
	
	protected void addLast(TermType term) {
		terms.addLast(term);
	}
	
	protected TermType pollLast() {
		return terms.pollLast();
	}
	
	protected void process(TermType term) {
		if(!isProcessingCompound())
			addLast(term);
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
	public void startCompoundName() {
		processingStack.push(createCompoundBuilder());
	}

	@Override
	public void endCompoundArgs() {
		process(processingStack.pop().build());
	}

	protected abstract TermBuilder<TermType> createCompoundBuilder();
	
}
