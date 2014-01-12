package org.jpc.engine.embedded.database;

import org.jpc.term.Term;

import com.google.common.base.Function;

public class IndexDescriptorAdapter {

	private final IndexDescriptor indexDescriptor;
	private final Function<Term, Term> adaptingFunction;
	
	public IndexDescriptorAdapter(IndexDescriptor indexDescriptor, Function<Term, Term> adaptingFunction) {
		this.indexDescriptor = indexDescriptor;
		this.adaptingFunction = adaptingFunction;
	}
}
