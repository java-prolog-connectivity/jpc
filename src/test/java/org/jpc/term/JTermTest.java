package org.jpc.term;

import static java.util.Arrays.asList;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import org.jpc.DefaultJpc;
import org.jpc.Jpc;
import org.junit.Test;

public class JTermTest {

	@Test
	public void testDifferentReferences() {
		Jpc jpc = new DefaultJpc();
		//s1 and s2 are equals but have different references.
		String s1 = "hello";
		String s2 = new String(s1);
		Term term1 = jpc.newWeakJTerm(s1);
		Term term2 = jpc.newWeakJTerm(s2);
		assertFalse(term1.equals(term2));
		String stringFromTerm = jpc.fromTerm(term2);
		assertFalse(stringFromTerm == s1);
		assertEquals(stringFromTerm, s1);
		assertTrue(stringFromTerm == s2);
	}

	@Test
	public void testWeakJTerm() {
		Jpc jpc = new DefaultJpc();
		Object o = new Object();
		Compound term = new Compound("x", asList(new Atom(""))); //arbitrary compound that will be associated to an object reference.
		jpc.newWeakJTerm(o, term); //associating the compound to a reference.
		assertEquals(term, jpc.toTerm(o));
		assertTrue(o == jpc.fromTerm(term));
		o = null;
		System.gc();
		try {
			jpc.fromTerm(term);
			fail();
		} catch(RuntimeException e) {}
	}
	
	@Test
	public void testForgetWeakJTerm() {
		Jpc jpc = new DefaultJpc();
		Object o = new Object();
		Compound term = new Compound("x", asList(new Atom(""))); //arbitrary compound that will be associated to an object reference.
		jpc.newWeakJTerm(o, term); //associating the compound to a reference.
		jpc.forgetJTerm(term);
		try {
			jpc.toTerm(o);
			fail();
		} catch(RuntimeException e) {}
		try {
			jpc.fromTerm(term);
			fail();
		} catch(RuntimeException e) {}
		
		jpc.newWeakJTerm(o, term); //associating again the compound to a reference.
		assertTrue(o == jpc.fromTerm(term));
	}
	

	@Test
	public void testGeneratedWeakJTerm() {
		Jpc jpc = new DefaultJpc();
		Object o = new Object();
		Compound term = jpc.newWeakJTerm(o); //associating the compound to a reference.
		assertEquals(term, jpc.toTerm(o));
		assertTrue(o == jpc.fromTerm(term));
		o = null;
		System.gc();
		try {
			jpc.fromTerm(term);
			fail();
		} catch(RuntimeException e) {}
	}
	
	@Test
	public void testForgetGeneratedWeakJTerm() {
		Jpc jpc = new DefaultJpc();
		Object o = new Object();
		Compound term = jpc.newWeakJTerm(o); //associating the compound to a reference.
		jpc.forgetJTerm(term);
		try {
			jpc.toTerm(o); //the object is not present anymore in the local table.
			fail();
		} catch(RuntimeException e) {}
		//still works since objects with a jpc generated term representation are maintained in the global table until they are garbage collected. 
		//when a term is resolved, it will look first at the local (context-scoped) table, if not found it will look at the global table (only jpc generated terms are stored in the global table).
		jpc.fromTerm(term); 
		o = null;
		System.gc(); //forcing garbage collection.
		try {
			jpc.fromTerm(term);
			fail();
		} catch(RuntimeException e) {}
		
		o = new Object();
		jpc.newWeakJTerm(o, term); //associating again the compound to a reference.
		jpc.forgetJTermRef(o);
		try {
			jpc.fromTerm(term);
			fail();
		} catch(RuntimeException e) {}
	}
	
	@Test
	public void testJTerm() {
		Jpc jpc = new DefaultJpc();
		Object o = new Object();
		Compound term = new Compound("x", asList(new Atom(""))); //arbitrary compound that will be associated to an object reference.
		jpc.newJTerm(o, term); //associating the compound to a reference.
		assertEquals(term, jpc.toTerm(o));
		assertTrue(o == jpc.fromTerm(term));
		o = null;
		System.gc();
		assertNotNull(jpc.fromTerm(term)); //it still should work, since the reference is maintained.
	}
	
	@Test
	public void testForgetJTerm() {
		Jpc jpc = new DefaultJpc();
		Object o = new Object();
		Compound term = new Compound("x", asList(new Atom(""))); //arbitrary compound that will be associated to an object reference.
		jpc.newJTerm(o, term); //associating the compound to a reference.
		jpc.forgetJTerm(term);
		try {
			jpc.fromTerm(term);
			fail();
		} catch(RuntimeException e) {}
		
		jpc.newJTerm(o, term); //associating again the compound to a reference.
		assertTrue(o == jpc.fromTerm(term));
	}
	
	@Test
	public void testGeneratedJTerm() {
		Jpc jpc = new DefaultJpc();
		Object o = new Object();
		Compound term = jpc.newJTerm(o); //associating the compound to a reference.
		assertEquals(term, jpc.toTerm(o));
		assertTrue(o == jpc.fromTerm(term));
		o = null;
		System.gc();
		assertNotNull(jpc.fromTerm(term)); //it still should work, since the reference is maintained.
	}
	
	@Test
	public void testForgetGeneratedJTerm() {
		Jpc jpc = new DefaultJpc();
		Object o = new Object();
		Compound term = jpc.newJTerm(o); //associating the compound to a reference.
		jpc.fromTerm(term);
		jpc.forgetJTerm(term);
		jpc.fromTerm(term); //generated term representations are maintained in the global table until garbage collected.
		o = null;
		System.gc(); //forcing garbage collection.
		try {
			jpc.fromTerm(term);
			fail();
		} catch(RuntimeException e) {}
	}
	
}
