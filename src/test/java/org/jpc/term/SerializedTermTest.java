package org.jpc.term;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;

import org.jpc.Jpc;
import org.jpc.JpcBuilder;
import org.junit.Test;

public class SerializedTermTest {

	private Jpc jpc = JpcBuilder.create().build();
	
	@Test
	public void testSerializedTerm() {
		String s = "hello";
		Term term = SerializedTerm.serialize(s);
		String s2 = jpc.fromTerm(term);
		assertFalse(s == s2);
		assertEquals(s, s2);
	}
	
}
