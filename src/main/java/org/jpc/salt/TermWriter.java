package org.jpc.salt;

import java.util.ArrayList;
import java.util.Deque;
import java.util.LinkedList;
import java.util.List;

public abstract class TermWriter<TermType> implements ContentHandler {
	
	private List<TermType> terms = new ArrayList<>();
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
	public void startCompound() {
		processingStack.push(createCompoundBuilder());
	}

	@Override
	public void endCompound() {
		process(processingStack.pop().build());
	}
	
	protected abstract TermBuilder<TermType> createCompoundBuilder();
	
}
