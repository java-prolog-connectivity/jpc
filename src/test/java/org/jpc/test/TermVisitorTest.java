package org.jpc.test;

import static java.util.Arrays.asList;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.HashMap;
import java.util.Map;

import org.jpc.engine.visitor.JpcWriterVisitor;
import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.Term;
import org.jpc.term.TermConvertable;
import org.jpc.term.Variable;
import org.jpc.util.LogicUtil;
import org.junit.Test;

/**
 * Testing the methods that use visitors
 * @author sergioc78
 *
 */
public class TermVisitorTest {

	Term t0 = new Compound(new Variable("C"), asList(new Variable("_")));
	Term t1 = new Compound(new Variable("A"), asList(new Compound(new Variable("_"), asList(LogicUtil.termsToList(new Variable("C"), new Atom("D"), new Variable("C"), new Variable("_"), new Variable("E"))))));
	
	@Test
	public void testVariables() {
		assertTrue(t1.hasVariable("A"));
		assertTrue(t1.hasVariable("_"));
		assertTrue(t1.hasVariable("C"));
		assertTrue(t1.hasVariable("E"));
		assertFalse(t1.hasVariable("D"));
		assertEquals(asList("A", "_", "C", "E"), t1.getVariablesNames());
		assertEquals(asList("A", "C", "E"), t1.nonAnonymousVariablesNames());
	}
	
	@Test
	public void testChangeVariablesNames() {
		Map<String, String> map = new HashMap<String, String>(){{
			put("_", "ANONYMOUS");
		}};
		Variable anon = new Variable("ANONYMOUS");
		assertEquals(anon, new Variable("_").changeVariablesNames(map));
		
		assertEquals(new Compound(new Variable("C"), asList(anon)), t0.changeVariablesNames(map));
		
		assertEquals(new Compound(new Variable("A"), asList(new Compound(anon, asList(LogicUtil.termsToList(new Variable("C"), new Atom("D"), new Variable("C"), anon, new Variable("E")))))), 
				t1.changeVariablesNames(map));
	}
	
	@Test
	public void testReplaceVariables() {
		final Term replacementTerm = new Compound(new Variable("C"), asList(new Atom("C")));
		Map<String, TermConvertable> map = new HashMap<String, TermConvertable>(){{
			put("C", replacementTerm);
		}};
		
		//not using Term.equals because that method fails when comparing two anonymous variables (they should not be considered equals)
		System.out.println(new Compound(new Compound(new Variable("C"), asList(new Atom("C"))), asList(new Variable("_"))));
		System.out.println(t0);
		System.out.println(t0.replaceVariables(map));
		assertTrue(new Compound(new Compound(new Variable("C"), asList(new Atom("C"))), asList(new Variable("_"))).termEquals(t0.replaceVariables(map)));
		assertTrue(new Compound(new Variable("A"), asList(new Compound(new Variable("_"), asList(LogicUtil.termsToList(replacementTerm, new Atom("D"), replacementTerm, new Variable("_"), new Variable("E")))))).
				termEquals(t1.replaceVariables(map)));
	}
	
	@Test
	public void testTermWriter() {
		JpcWriterVisitor termWriter = new JpcWriterVisitor();
		t1.accept(termWriter);
		assertTrue(t1.termEquals(termWriter.terms().get(0)));
	}

}
