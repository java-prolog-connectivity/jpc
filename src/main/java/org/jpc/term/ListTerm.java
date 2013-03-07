package org.jpc.term;

import static java.util.Arrays.asList;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.jpc.converter.TermConvertable;
import org.jpc.salt.TermContentHandler;

public class ListTerm extends ArrayList<Term> implements TermConvertable {
	
	public static Term listTerm(Term... terms) {
		return create(terms).asTerm();
	}

	public static Term listTerm(List<? extends Term> terms) {
		return create(terms).asTerm();
	}
	
	public static ListTerm create(Term... terms) {
		return new ListTerm(asList(terms));
	}
	
	public static ListTerm create(List<? extends Term> terms) {
		return new ListTerm(terms);
	}
	
	public ListTerm() {
	}
	
	public ListTerm(Collection<? extends Term> terms) {
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
	
	/**
	 * Transforms this list of TermConvertables to a list of Terms
	 * @return
	 */
//	public List<Term> asTerms() {
//		List<Term> terms = new ArrayList<>();
//		for(TermConvertable termConvertable : this) {
//			terms.add(termConvertable.asTerm());
//		}
//		return terms;
//	}
	
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
		for(int i=0; i<size(); i++) {
			Term item = get(i);
			sb.append(item == this?"(this List)":item.toString());
			if(i<size()-1)
				sb.append(", ");
		}
		sb.append("]");
		return sb.toString();
	}

	public void readEach(TermContentHandler contentHandler) {
		for(Term each : this) {
			each.read(contentHandler);
		}
	}
	
}
