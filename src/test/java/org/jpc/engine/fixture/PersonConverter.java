package org.jpc.engine.fixture;

import static java.util.Arrays.asList;

import java.lang.reflect.Type;

import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.converter.ToTermConverter;
import org.jpc.term.Atom;
import org.jpc.term.Compound;

public class PersonConverter implements FromTermConverter<Compound, Person>, 
	ToTermConverter<Person, Compound> {

	public static final String PERSON_FUNCTOR_NAME = "person";
	
	@Override
	public Compound toTerm(Person person, Class<Compound> termClass, Jpc context) {
		return new Compound(PERSON_FUNCTOR_NAME, asList(new Atom(person.getName())));
	}

	@Override
	public Person fromTerm(Compound personTerm, Type targetType, Jpc context) {
		String name = ((Atom)((Compound)personTerm).arg(1)).getName();
		return new Person(name);
	}

}
