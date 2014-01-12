package org.jpc.term.jterm;

import static java.util.Arrays.asList;

import org.jpc.converter.TermConvertable;
import org.jpc.term.Compound;
import org.jpc.term.IntegerTerm;

/**
 * A system-generated term representation of a Java object reference.
 * @author sergioc
 *
 */
public class JTermId implements TermConvertable<Compound> {

	public static final String JTERM_FUNCTOR_NAME = "jterm";
	
	private final int id; //numeric identifier uniquely associated with the JTerm reference.
	private final Compound term; //compound term mapped to the JTerm reference.
	
	public JTermId(int id) {
		this.id = id;
		term = new Compound(JTERM_FUNCTOR_NAME, asList(new IntegerTerm(id)));
	}

	public int getId() {
		return id;
	}

	@Override
	public Compound asTerm() {
		return term;
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
		JTermId other = (JTermId) obj;
		if (id != other.id)
			return false;
		return true;
	}
	
}
