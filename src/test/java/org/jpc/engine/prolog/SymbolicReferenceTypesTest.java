package org.jpc.engine.prolog;

import static java.util.Arrays.asList;
import static org.jpc.engine.fixture.Student.STUDENT_FUNCTOR_NAME;
import static org.jpc.engine.prolog.PrologEngines.defaultPrologEngine;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import org.jconverter.converter.ConversionException;
import org.jpc.Jpc;
import org.jpc.JpcBuilder;
import org.jpc.engine.fixture.Person;
import org.jpc.engine.fixture.PersonConverter;
import org.jpc.query.Query;
import org.jpc.query.Solution;
import org.jpc.term.Compound;
import org.jpc.term.IntegerTerm;
import org.jpc.term.SerializedTerm;
import org.jpc.term.Term;
import org.jpc.term.Var;
import org.junit.Before;
import org.junit.Test;

/**
 *  Note that all the tests relying on the System.gc() method are intended as a demo of JPC features.
 * It is not possible to "force" the garbage collector to execute atomically, the best we can do is give it a hint to run by means of the System.gc() method.
 * Theoretically, some of the tests relying on that method may fail sometimes. In practice, this almost never happens.  
 * @author sergioc
 *
 */
public class SymbolicReferenceTypesTest {

	@Before
	public void retractStudents() {
		defaultPrologEngine().retractAll(new Compound(STUDENT_FUNCTOR_NAME, asList(Var.ANONYMOUS_VAR)));
	}
	
	@Test
	public void testWhiteBoxAndEqualityLowLevel() {
		PrologEngine prologEngine = defaultPrologEngine();
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
		PrologEngine prologEngine = defaultPrologEngine();
		Person person = new Person("Mary");
		Jpc ctx = JpcBuilder.create().register(new PersonConverter()).build();
		prologEngine.assertz(new Compound(STUDENT_FUNCTOR_NAME, asList(ctx.toTerm(person))));
		Query query = prologEngine.query(new Compound(STUDENT_FUNCTOR_NAME, asList(new Var("Person"))), ctx);
		Person queriedPerson = query.<Person>selectObject("Person").oneSolutionOrThrow();
		assertEquals(person, queriedPerson);
		assertFalse(person == queriedPerson);
	}
	
	@Test
	public void testWhiteBoxAndIdentity() {
		PrologEngine prologEngine = defaultPrologEngine();
		Person person = new Person("Mary");
		Jpc ctx = JpcBuilder.create().register(new PersonConverter()).build();
		Term personTerm = ctx.newJRefTerm(person, ctx.<Compound>toTerm(person));
		prologEngine.assertz(new Compound(STUDENT_FUNCTOR_NAME, asList(personTerm)));
		Query query = prologEngine.query(new Compound(STUDENT_FUNCTOR_NAME, asList(new Var("Person"))), ctx);
		Person queriedPerson = query.<Person>selectObject("Person").oneSolutionOrThrow();
		assertEquals(person, queriedPerson);
		assertTrue(person == queriedPerson);
	}
	
	@Test
	public void testBlackBoxAndEqualityBySerialization() {
		PrologEngine prologEngine = defaultPrologEngine();
		Person person = new Person("Mary");
		prologEngine.assertz(new Compound(STUDENT_FUNCTOR_NAME, asList(SerializedTerm.serialize(person))));
		Query query = prologEngine.query(new Compound(STUDENT_FUNCTOR_NAME, asList(new Var("Person"))));
		Person queriedPerson = query.<Person>selectObject("Person").oneSolutionOrThrow();
		assertEquals(person, queriedPerson);
		assertFalse(person == queriedPerson);
	}
	
	@Test
	public void testBlackBoxAndIdentityAdHoc() {
		PrologEngine prologEngine = defaultPrologEngine();
		Person person = new Person("Mary");
		Jpc ctx = JpcBuilder.create().build();
		Term personTerm = ctx.newJRefTerm(person, new Compound("cool_student", asList(new IntegerTerm(42))));
		prologEngine.assertz(new Compound(STUDENT_FUNCTOR_NAME, asList(personTerm)));
		Query query = prologEngine.query(new Compound(STUDENT_FUNCTOR_NAME, asList(new Var("Person"))), ctx);
		Person queriedPerson = query.<Person>selectObject("Person").oneSolutionOrThrow();
		assertEquals(person, queriedPerson);
		assertTrue(person == queriedPerson);
	}
	
	@Test
	public void testBlackBoxAndIdentityGenerated() {
		PrologEngine prologEngine = defaultPrologEngine();
		Person person = new Person("Mary");
		Jpc ctx = JpcBuilder.create().build();
		Term personTerm = ctx.newJRefTerm(person);
		prologEngine.assertz(new Compound(STUDENT_FUNCTOR_NAME, asList(personTerm)));
		Query query = prologEngine.query(new Compound(STUDENT_FUNCTOR_NAME, asList(new Var("Person"))), ctx);
		Person queriedPerson = query.<Person>selectObject("Person").oneSolutionOrThrow();
		assertEquals(person, queriedPerson);
		assertTrue(person == queriedPerson);
	}
	
	@Test
	public void testExplicitManagementLifeSpan() {
		PrologEngine prologEngine = defaultPrologEngine();
		Person person = new Person("Mary");
		Jpc ctx = JpcBuilder.create().register(new PersonConverter()).build();
		Term personTerm = ctx.newJRefTerm(person, ctx.<Compound>toTerm(person));
		prologEngine.assertz(new Compound(STUDENT_FUNCTOR_NAME, asList(personTerm)));
		assertTrue(person == prologEngine.query(new Compound(STUDENT_FUNCTOR_NAME, asList(new Var("Person"))), ctx).selectObject("Person").oneSolutionOrThrow());
		ctx.forgetJRefTerm((Compound)personTerm);
		Person queriedPerson = prologEngine.query(new Compound(STUDENT_FUNCTOR_NAME, asList(new Var("Person"))), ctx).<Person>selectObject("Person").oneSolutionOrThrow();
		assertEquals(person, queriedPerson);
		assertFalse(person == queriedPerson);
	}
	
	@Test
	public void testGarbageCollectionLifeSpan() {
		PrologEngine prologEngine = defaultPrologEngine();
		Person person = new Person("Mary");
		Jpc ctx = JpcBuilder.create().register(new PersonConverter()).build();
		Term personTerm = ctx.newWeakJRefTerm(person, ctx.<Compound>toTerm(person));
		prologEngine.assertz(new Compound(STUDENT_FUNCTOR_NAME, asList(personTerm)));
		person = null;
		System.gc();
		try {
			prologEngine.query(new Compound(STUDENT_FUNCTOR_NAME, asList(new Var("Person")))).<Person>selectObject("Person").oneSolutionOrThrow();
			fail();
		} catch(ConversionException e) {}
	}
	
}
