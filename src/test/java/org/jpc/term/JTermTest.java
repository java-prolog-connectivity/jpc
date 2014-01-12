package org.jpc.term;

import static java.util.Arrays.asList;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import org.jpc.DefaultJpc;
import org.jpc.Jpc;
import org.junit.Test;

public class JTermTest {

	private Jpc jpc = new DefaultJpc();
	
	@Test
	public void testEquality() {
		Object o = new Object();
		assertEquals(new JRef(o), new JRef(o));
		assertFalse(new JRef(o).equals(new JRef(new Object())));
		Compound compound1 = new Compound("jref", asList(new JRef(o)));
		Compound compound2 = new Compound("jref", asList(new JRef(o)));
		Compound compound3 = new Compound("jref", asList(new JRef(new Object())));
		assertEquals(compound1, compound2);
		assertFalse(compound1.equals(compound3));
	}
	
	@Test
	public void testJTerm() {
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
	public void testRemoveJTerm() {
		Object o = new Object();
		Compound term = new Compound("y", asList(new Atom(""))); //arbitrary compound that will be associated to an object reference.
		jpc.newWeakJTerm(o, term);
		jpc.forgetJTerm(term);
		try {
			jpc.fromTerm(term);
			fail();
		} catch(RuntimeException e) {}
		
	}
	
	@Test
	public void testJRef() {
		Object o = new Object();
		Term jRef = jpc.newWeakJTerm(o);
		assertTrue(o == jpc.fromTerm(jRef));
		o = null;
		System.gc();
		try {
			jpc.fromTerm(jRef);
			fail();
		} catch(RuntimeException e) {}
	}
	
	@Test
	public void testJRef2() {
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
	public void testSerializedTerm() {
		String s = "hello";
		Term term = SerializedTerm.serialize(s);
		String s2 = jpc.fromTerm(term);
		assertFalse(s == s2);
		assertEquals(s, s2);
	}
	
}
