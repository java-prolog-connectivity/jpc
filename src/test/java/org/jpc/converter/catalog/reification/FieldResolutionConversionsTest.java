package org.jpc.converter.catalog.reification;

import static java.util.Arrays.asList;
import static org.junit.Assert.assertEquals;

import org.jpc.Jpc;
import org.jpc.JpcBuilder;
import org.jpc.converter.catalog.reification.ReificationFixture.B;
import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.Term;
import org.junit.Test;
import org.minitoolbox.reflection.reification.StaticClass;


public class FieldResolutionConversionsTest {
	
	private static Jpc jpc = JpcBuilder.create().build();
	
	@Test
	public void testStaticMethodCall() {
		Term classTerm = jpc.toTerm(new StaticClass(B.class));
		Term fieldTerm = new Atom("m");
		Term fieldResolutionTerm = new Compound(FieldResolutionConverter.FIELD_RESOLUTION_OPERATOR, asList(classTerm, fieldTerm));
		assertEquals(10L, jpc.fromTerm(fieldResolutionTerm));
	}

}
