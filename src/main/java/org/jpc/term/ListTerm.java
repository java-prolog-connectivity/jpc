package org.jpc.term;

import static java.util.Arrays.asList;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;

public class ListTerm<E extends TermConvertable> extends ArrayList<E> implements TermConvertable {

	public static <E extends TermConvertable> ListTerm listTerm(E ...terms) {
		return new ListTerm(Arrays.asList(terms));
	}
	
	public ListTerm() {
		super();
	}
	
	public ListTerm(Collection<? extends E> terms) {
		super(terms);
	}
	
	public ListTerm(int initialCapacity) {
		super(initialCapacity);
	}
	
	@Override
	public Term asTerm() {
		Term unwrappedListTerm = Atom.EMPTY_LIST;
		for (int i = size() - 1; i >= 0; --i) {
			unwrappedListTerm = new Compound(".", asList(get(i), unwrappedListTerm));
		}
		return unwrappedListTerm;
	}
	
	
	@Override
	public int hashCode() {
		return asTerm().hashCode();
	}
	
	@Override
	public boolean equals(Object obj) {
		return (this == obj || (obj instanceof ListTerm && asTerm().equals(((ListTerm)obj).asTerm())));
	}
	
	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder("[");
		for(TermConvertable termConvertable : this) {
			sb.append(termConvertable.asTerm().toString());
		}
		sb.append("]");
		return sb.toString();
	}

}
