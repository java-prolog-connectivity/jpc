package org.jpc.util.salt;

import java.util.ArrayDeque;
import java.util.Deque;
import java.util.function.Consumer;


public abstract class TermStreamer<TermType> implements TermContentHandler {

	protected final Consumer<TermType> termProcessor;

	/**
	 * An auxiliar stack for storing partially built terms (like compounds)
	 */
	private final Deque<TermBuilder<TermType>> processingStack = new ArrayDeque<>();

/*	public TermStreamer() {
		this((term) -> {});
	}*/

	public TermStreamer(Consumer<TermType> termProcessor) {
		this.termProcessor = termProcessor;
	}

	private boolean isProcessingCompound() {
		return !processingStack.isEmpty();
	}

	protected void process(TermType term) {
		if(!isProcessingCompound()) {
			termProcessor.accept(term);
		} else {
			addToProcessingStack(term);
		}
	}
	
	private void addToProcessingStack(TermType term) {
		TermBuilder<TermType> peekCompoundBuilder = processingStack.peek();
		if(!peekCompoundBuilder.hasFunctor()) {
			peekCompoundBuilder.setFunctor(term);
		} else {
			peekCompoundBuilder.addArg(term);
		}
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
