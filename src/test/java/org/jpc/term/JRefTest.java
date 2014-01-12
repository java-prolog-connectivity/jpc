package org.jpc.term;

import static java.util.Arrays.asList;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;

import org.junit.Test;

public class JRefTest {

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
	
}
