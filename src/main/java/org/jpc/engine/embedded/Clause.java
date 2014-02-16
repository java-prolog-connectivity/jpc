package org.jpc.engine.embedded;

import org.jpc.JpcException;
import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.Term;

/**
 * A Prolog clause.
 * In its simples form, a clause just wraps a compiled term (a fact) and a unique id.
 * @see Rule
 * @author sergioc
 *
 */
public class Clause implements Comparable<Clause> {
	
	protected void checkClauseHead(Term head) {
		if(!(head instanceof Atom || head instanceof Compound)) //only atoms and compounds are allowed in the clause head.
			throw new JpcException("Invalid clause head: " + head + ".");
	}
	
	private final int id;
	private final Term head;
	
	public Clause(Term head, int id) {
		checkClauseHead(head);
		this.id = id;
		this.head = head.compile(id);
	}
	
	public int getId() {
		return id;
	}
	
	public Term getHead() {
		return head;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + id;
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		Clause other = (Clause) obj;
		if (id != other.id)
			return false;
		return true;
	}

	@Override
	public int compareTo(Clause clause) {
		return new Integer(getId()).compareTo(clause.getId());
	}
	
}
