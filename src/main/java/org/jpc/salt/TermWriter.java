package org.jpc.salt;

import java.util.ArrayList;
import java.util.Deque;
import java.util.LinkedList;
import java.util.List;

public abstract class TermWriter<TermType> extends DefaultHandler {
	/**
	 * The terms that have been written using this TermWritter
	 * Subclasses could decide to stream each processed term somewhere else (e.g., asserting it in a logic engine) instead of storing them here
	 */
	private List<TermType> terms = new ArrayList<>();
	
	/**
	 * An auxiliar stack for storing partially built terms (like compounds)
	 */
	private Deque<TermBuilder<TermType>> processingStack = new LinkedList<>();

	private boolean isProcessingCompound() {
		return !processingStack.isEmpty();
	}
	
	public List<TermType> getTerms() {
		return terms;
	}
	
	protected void addTerm(TermType term) {
		terms.add(term);
	}
	
	protected void process(TermType term) {
		if(!isProcessingCompound())
			addTerm(term);
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
