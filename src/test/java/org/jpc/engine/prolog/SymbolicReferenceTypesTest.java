package org.jpc.engine.prolog;

import static java.util.Arrays.asList;
import static org.jpc.engine.provider.PrologEngineProviderManager.getPrologEngine;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;

import org.jpc.Jpc;
import org.jpc.JpcBuilder;
import org.jpc.engine.fixture.Person;
import org.jpc.engine.fixture.PersonConverter;
import org.jpc.query.Query;
import org.jpc.query.Solution;
import org.jpc.term.Compound;
import org.jpc.term.Term;
import org.jpc.term.Var;
import org.junit.Test;

public class SymbolicReferenceTypesTest {

	private static final String STUDENT_FUNCTOR_NAME = "student";
	
	@Test
	public void testWhiteBoxAndEqualityLowLevel() {
		PrologEngine prologEngine = getPrologEngine();
		Person person = new Person("Mary");
		prologEngine.assertz(new Compound(STUDENT_FUNCTOR_NAME, asList(person.toTerm())));
		Query query = prologEngine.query(new Compound(STUDENT_FUNCTOR_NAME, asList(new Var("Person"))));
		Solution solution = query.oneSolutionOrThrow();
		Term personTerm = solution.get("Person");
		Person queriedPerson = Person.create(personTerm);
		assertEquals(person, queriedPerson);
		assertFalse(person == queriedPerson);
	}
	
	@Test
	public void testWhiteBoxAndEquality() {
		PrologEngine prologEngine = getPrologEngine();
		Person person = new Person("Mary");
		Jpc ctx = JpcBuilder.create().register(new PersonConverter()).build();
		prologEngine.assertz(new Compound(STUDENT_FUNCTOR_NAME, asList(ctx.toTerm(person))));
		Query query = prologEngine.query(new Compound(STUDENT_FUNCTOR_NAME, asList(new Var("Person"))), ctx);
		Person queriedPerson = query.<Person>selectObject("Person").oneSolutionOrThrow();
		assertEquals(person, queriedPerson);
		assertFalse(person == queriedPerson);
	}
	
//	@Test
//	public void testWhiteBoxAndIdentity() {
//		Jpc ctx = JpcBuilder.create().build();
//		Person person = new Person("Mary");
//		ctx.newJTerm(person, compound);
//	}
	
}
