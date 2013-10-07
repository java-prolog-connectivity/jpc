package org.jpc.term.jterm;

import static java.util.Arrays.asList;

import org.jpc.converter.TermConvertable;
import org.jpc.term.Compound;
import org.jpc.term.IntegerTerm;

public class JRefId implements TermConvertable<Compound> {

	public static final String JREF_FUNCTOR = "jref";
	
	int id;

	public JRefId(int id) {
		this.id = id;
	}

	public int getId() {
		return id;
	}

	@Override
	public Compound asTerm() {
		return new Compound(JREF_FUNCTOR, asList(new IntegerTerm(id)));
	}
	
	@Override
	public String toString() {
		return "@"+id;
	}
	
	@Override
	public int hashCode() {
		return id;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		JRefId other = (JRefId) obj;
		if (id != other.id)
			return false;
		return true;
	}
	
}
