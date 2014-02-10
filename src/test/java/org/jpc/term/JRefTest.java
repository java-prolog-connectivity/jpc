package org.jpc.term;

import static java.util.Arrays.asList;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;

import org.junit.Test;

public class JRefTest {

	@Test
	public void testEquality() {
		Object o = new Object();
		assertEquals(JRef.jref(o), JRef.jref(o));
		assertFalse(JRef.jref(o).equals(JRef.jref(new Object())));
		Compound compound1 = new Compound("jref", asList(JRef.jref(o)));
		Compound compound2 = new Compound("jref", asList(JRef.jref(o)));
		Compound compound3 = new Compound("jref", asList(JRef.jref(new Object())));
		assertEquals(compound1, compound2);
		assertFalse(compound1.equals(compound3));
	}
	
}
