package org.jpc.engine.prolog;

import static java.util.Arrays.asList;
import static org.jpc.engine.prolog.PrologEngines.defaultPrologEngine;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;

import org.jpc.query.Query;
import org.jpc.query.Solution;
import org.jpc.term.Compound;
import org.jpc.term.SerializedTerm;
import org.jpc.term.Term;
import org.jpc.term.Var;
import org.jpc.term.jterm.JTermManager;
import org.junit.Test;

public class AnsweringJavaObjectsTest {

	@Test
	public void testObjectReference() {
		String s1 = new String("x"); //it should not use the interned string "x" in order to avoid mixing the cleaning of dead references across different tests using by chance the same string object.
		Query query = defaultPrologEngine().query(new Compound("=", Arrays.<Term>asList(new Var("A"), JTermManager.weakJTerm(s1))));
		String s2 = query.<String>selectObject("A").oneSolutionOrThrow();
		assertTrue(s1 == s2);
	}

	@Test
	public void testObjectSerialization() {
		String s1 = new String("x");
		Query query = defaultPrologEngine().query(new Compound("=", Arrays.<Term>asList(new Var("A"), SerializedTerm.serialize(s1))));
		Object s2 = query.selectObject("A").oneSolutionOrThrow();
		assertEquals(s1, s2);
		assertFalse(s1 == s2);
	}
	
	@Test
	public void testQueryArguments() {
		String s1 = new String("x");
		Query query = defaultPrologEngine().query("A = ?1/m, B = ?1, C = ?1/r, D = ?1/s", asList(s1));
		Solution solution = query.oneSolutionOrThrow();
		String a = solution.getObject("A");
		String b = solution.getObject("B");
		String c = solution.getObject("C");
		String d = solution.getObject("D");
		
		assertEquals(s1, a);
		assertEquals(s1, b);
		assertEquals(s1, c);
		assertEquals(s1, d);
		
		assertFalse(s1==a);
		assertFalse(s1==b);
		assertTrue(s1==c);
		assertFalse(s1==d);
	}

}
