package org.jpc.mapping.converter.catalog.error;

import org.jpc.Jpc;
import org.jpc.JpcBuilder;
import org.jpc.term.Compound;
import org.junit.Test;

public class ErrorConversionsTest {

	Jpc jpc = JpcBuilder.create().build();
	
	@Test
	public void testJException() {
		try {
			throw new RuntimeException();
		} catch(Exception e) {
			Compound exceptionTerm = jpc.toTerm(e);
			//TODO complete
		}
	}
}
