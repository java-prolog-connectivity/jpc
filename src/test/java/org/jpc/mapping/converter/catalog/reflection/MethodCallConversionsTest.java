package org.jpc.mapping.converter.catalog.reflection;

import static java.util.Arrays.asList;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import org.jpc.Jpc;
import org.jpc.JpcBuilder;
import org.jpc.mapping.converter.catalog.reflection.ReificationFixture.B;
import org.jpc.engine.logtalk.LogtalkConstants;
import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.Integer;
import org.jpc.term.Term;
import org.jpc.util.reification.ReflectiveClass;
import org.junit.Test;



public class MethodCallConversionsTest {

	private static Jpc jpc = JpcBuilder.create().build();
	
	@Test
	public void testNew() {
		Term classTerm = jpc.toTerm(new ReflectiveClass(B.class));
		Term messageTerm = new Atom("new");
		Term methodCallTerm = new Compound(LogtalkConstants.LOGTALK_OPERATOR, asList(classTerm, messageTerm));
		assertTrue(jpc.fromTerm(methodCallTerm) instanceof B);
	}
	
	@Test
	public void testStaticMethodCall() {
		Term classTerm = jpc.toTerm(new ReflectiveClass(B.class));
		Term messageTerm = new Compound("m", asList(new Integer(10)));
		Term methodCallTerm = new Compound(LogtalkConstants.LOGTALK_OPERATOR, asList(classTerm, messageTerm));
		assertEquals(new Long(10L), jpc.fromTerm(methodCallTerm));
	}
	
}
