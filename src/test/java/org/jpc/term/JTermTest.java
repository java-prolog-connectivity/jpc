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
		String s2 = new String("hello");
		jpc.newWeakJTerm(s1);
		Term jRef2 = jpc.newWeakJTerm(s2);
		String stringFromTerm = jpc.fromTerm(jRef2);
		assertFalse(stringFromTerm == s1);
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
			jpc.fromTerm(term);
			fail();
		} catch(RuntimeException e) {}
		
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
		jpc.forgetJTermRef(o);
		try {
			jpc.fromTerm(term);
			fail();
		} catch(RuntimeException e) {}
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
			jpc.fromTerm(term);
			fail();
		} catch(RuntimeException e) {}
		
		jpc.newWeakJTerm(o, term); //associating again the compound to a reference.
		jpc.forgetJTermRef(o);
		try {
			jpc.fromTerm(term);
			fail();
		} catch(RuntimeException e) {}
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
		jpc.forgetJTerm(term);
		try {
			jpc.fromTerm(term);
			fail();
		} catch(RuntimeException e) {}
		
		jpc.newJTerm(o, term); //associating again the compound to a reference.
		jpc.forgetJTermRef(o);
		try {
			jpc.fromTerm(term);
			fail();
		} catch(RuntimeException e) {}
	}
	
}
