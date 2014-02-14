package org.jpc.engine.fixture;

import static java.util.Arrays.asList;

import java.io.Serializable;

import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.Term;

public  class Person implements Serializable {
	
	public static Person create(Term term) {
		String name = ((Atom)((Compound)term).arg(1)).getName();
		return new Person(name);
	}
	
	public static final String PERSON_FUNCTOR_NAME = "person";
	
	private final String name;
	public Person(String name) {this.name = name;}
	
	public String getName() {return name;}
	
	public Term toTerm() {
		return new Compound(PERSON_FUNCTOR_NAME, asList(new Atom(name)));
	}
	
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((name == null) ? 0 : name.hashCode());
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
		Person other = (Person) obj;
		if (name == null) {
			if (other.name != null)
				return false;
		} else if (!name.equals(other.name))
			return false;
		return true;
	}	
}