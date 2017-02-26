package org.jpc.mapping.converter.catalog.reflection;

import static java.util.Arrays.asList;
import static org.jpc.term.JRef.jRef;
import static org.junit.Assert.assertEquals;

import org.jpc.Jpc;
import org.jpc.JpcBuilder;
import org.jpc.mapping.converter.catalog.reflection.ReificationFixture.B;
import org.jpc.engine.logtalk.LogtalkConstants;
import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.ListTerm;
import org.jpc.term.Term;
import org.jpc.util.reification.ReflectiveClass;
import org.junit.Test;


public class FieldResolutionConversionsTest {
	
	private static Jpc jpc = JpcBuilder.create().build();
	
	@Test
	public void testStaticField() {
		Term term = jpc.toTerm(new ReflectiveClass(B.class));
		Term fieldTerm = new Atom("m");
		Term fieldResolutionTermMessage = ListTerm.create(fieldTerm).asTerm();
		Term fieldResolutionTerm = new Compound(LogtalkConstants.LOGTALK_OPERATOR, asList(term, fieldResolutionTermMessage));
		assertEquals(new Long(10L), jpc.fromTerm(fieldResolutionTerm));
		Term mutatorTerm;
		mutatorTerm = jpc.listTerm("m", 12L).asTerm();
		jpc.fromTerm(new Compound(LogtalkConstants.LOGTALK_OPERATOR, asList(term, mutatorTerm)));
		assertEquals(12L, B.m);
	}

	@Test
	public void testInstanceField() {
		Object object = new B();
		Term term = jRef(object);
		Term fieldTerm = new Atom("n");
		Term fieldResolutionTermMessage = ListTerm.create(fieldTerm).asTerm();
		Term fieldResolutionTerm = new Compound(LogtalkConstants.LOGTALK_OPERATOR, asList(term, fieldResolutionTermMessage));
		assertEquals(new Long(10L), jpc.fromTerm(fieldResolutionTerm));
		Term mutatorTerm;
		mutatorTerm = jpc.listTerm("m", 12L).asTerm();
		jpc.fromTerm(new Compound(LogtalkConstants.LOGTALK_OPERATOR, asList(term, mutatorTerm)));
		assertEquals(12L, B.m);
	}
	
}
