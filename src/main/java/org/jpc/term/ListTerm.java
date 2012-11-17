package org.jpc.term;

public class ListTerm extends TermWrapper {

	public ListTerm(Term term) {
		super(term);
	}
	
	@Override
	public int hashCode() {
		return getWrappedTerm().hashCode();
	}
	
	@Override
	public boolean equals(Object obj) {
		return (this == obj || (obj instanceof ListTerm && getWrappedTerm().equals(((ListTerm)obj).getWrappedTerm())));
	}
	
}
