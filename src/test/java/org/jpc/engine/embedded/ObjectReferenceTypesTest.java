package org.jpc.engine.embedded;

import static java.util.Arrays.asList;
import static org.jpc.engine.fixture.Student.STUDENT_FUNCTOR_NAME;
import static org.jpc.engine.provider.PrologEngineProviderManager.getPrologEngine;
import static org.jpc.term.Var.dontCare;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.jpc.engine.fixture.Person;
import org.jpc.engine.prolog.PrologEngine;
import org.jpc.query.Solution;
import org.jpc.term.Compound;
import org.jpc.term.JRef;
import org.jpc.term.Var;
import org.junit.Before;
import org.junit.Test;

/**
 * Note that all the tests relying on the System.gc() method are intended as a demo of JPC features.
 * It is not possible to "force" the garbage collector to execute atomically, the best we can do is give it a hint to run by means of the System.gc() method.
 * Theoretically, some of the tests relying on that method may fail sometimes. In practice, this almost never happens.  
 * @author sergioc
 *
 */
public class ObjectReferenceTypesTest {

	@Before
	public void retractStudents() {
		getPrologEngine().retractAll(new Compound(STUDENT_FUNCTOR_NAME, asList(dontCare())));
	}
	
	@Test
	public void testConstantUnification() {
		Person person1 = new Person("Mary");
		Person person2 = new Person("Mary");
		PrologEngine prologEngine = getPrologEngine();
		assertFalse(prologEngine.query(new Compound(STUDENT_FUNCTOR_NAME, asList(dontCare()))).hasSolution());
		
		prologEngine.assertz(new Compound(STUDENT_FUNCTOR_NAME, asList(JRef.jRef(person1))));
		assertTrue(prologEngine.query(new Compound(STUDENT_FUNCTOR_NAME, asList(JRef.jRef(person1)))).hasSolution());
		assertTrue(prologEngine.query(new Compound(STUDENT_FUNCTOR_NAME, asList(JRef.jRef(person2)))).hasSolution());
	}
	
	@Test
	public void testIdentityPreservation() {
		Person person1 = new Person("Mary");
		PrologEngine prologEngine = getPrologEngine();
		assertFalse(prologEngine.query(new Compound(STUDENT_FUNCTOR_NAME, asList(dontCare()))).hasSolution());
		
		prologEngine.assertz(new Compound(STUDENT_FUNCTOR_NAME, asList(JRef.jRef(person1))));
		Solution solution = prologEngine.query(new Compound(STUDENT_FUNCTOR_NAME, asList(new Var("X")))).oneSolutionOrThrow();
		JRef<Person> jRef = (JRef<Person>) solution.get("X");
		assertTrue(person1 == jRef.getReferent());
	}
	
	@Test
	public void testLifeSpan() {
		Person person1 = new Person("Mary");
		Person person2 = new Person("Mary");
		Person person3 = new Person("Mary");
		PrologEngine prologEngine = getPrologEngine();
		assertFalse(prologEngine.query(new Compound(STUDENT_FUNCTOR_NAME, asList(dontCare()))).hasSolution());
		
		prologEngine.assertz(new Compound(STUDENT_FUNCTOR_NAME, asList(JRef.jRef(person1))));
		prologEngine.assertz(new Compound(STUDENT_FUNCTOR_NAME, asList(JRef.weakJRef(person2))));
		prologEngine.assertz(new Compound(STUDENT_FUNCTOR_NAME, asList(JRef.softJRef(person3))));
		
		assertEquals(3, prologEngine.query(new Compound(STUDENT_FUNCTOR_NAME, asList(JRef.jRef(person1)))).allSolutions().size());
		
		person2 = null;
		System.gc();
		assertEquals(2, prologEngine.query(new Compound(STUDENT_FUNCTOR_NAME, asList(JRef.jRef(person1)))).allSolutions().size());
	}
	
	@Test
	public void testCleaningTask() {
		Person person = new Person("Mary");
		final PrologEngine prologEngine = getPrologEngine();
		Runnable cleaningTask = new Runnable() {
			@Override
			public void run() {
				prologEngine.retractAll(new Compound(STUDENT_FUNCTOR_NAME, asList(dontCare())));
			}
		};
		prologEngine.assertz(new Compound(STUDENT_FUNCTOR_NAME, asList(JRef.weakJRef(person, cleaningTask))));
		assertTrue(prologEngine.query(new Compound(STUDENT_FUNCTOR_NAME, asList(dontCare()))).hasSolution());
		person = null;
		System.gc();
		assertFalse(prologEngine.query(new Compound(STUDENT_FUNCTOR_NAME, asList(dontCare()))).hasSolution());
	}
}
