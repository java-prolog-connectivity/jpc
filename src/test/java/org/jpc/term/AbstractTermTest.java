package org.jpc.term;

import static java.util.Arrays.asList;
import static org.jpc.term.Variable.ANONYMOUS_VAR;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.HashMap;
import java.util.Map;

import org.jpc.salt.JpcWriter;
import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.ListTerm;
import org.jpc.term.Term;
import org.jpc.term.TermConvertable;
import org.jpc.term.Variable;
import org.junit.Test;

/**
 * Testing the methods that query/transform terms using visitors
 * @author sergioc
 *
 */
public class AbstractTermTest {
	Term aAtom = new Atom("A");
	Term aVar = new Variable("A");
	Term bVar = new Variable("B");
	Term namedAnonVar = new Variable("_A");

	Term t0 = new Compound(aAtom, asList(ANONYMOUS_VAR));
	Term t1 = new Compound(aAtom, asList(new Compound(ANONYMOUS_VAR, asList(aVar))));
	Term t2 = new Compound(aAtom, asList(new Compound(ANONYMOUS_VAR, asList(
			new ListTerm(asList(aVar, aVar))))));
	Term t3 = new Compound(aAtom, asList(new Compound(ANONYMOUS_VAR, asList(
			new ListTerm(asList(aAtom, aVar, bVar, namedAnonVar, ANONYMOUS_VAR))))));
	
	
	@Test
	public void testHasVariable() {
		assertTrue(aVar.hasVariable("A"));
		assertFalse(aAtom.hasVariable("A"));
		assertTrue(namedAnonVar.hasVariable("_A"));
		assertFalse(namedAnonVar.hasVariable("_"));
		assertTrue(ANONYMOUS_VAR.hasVariable("_"));
		
		assertTrue(t0.hasVariable("_"));
		assertFalse(t0.hasVariable("A"));
		
		assertTrue(t1.hasVariable("_"));
		assertTrue(t1.hasVariable("A"));
		
		assertTrue(t2.hasVariable("_"));
		assertTrue(t2.hasVariable("A"));
		
		assertTrue(t3.hasVariable("_"));
		assertTrue(t3.hasVariable("A"));
		assertTrue(t3.hasVariable("B"));
		assertTrue(t3.hasVariable("_A"));
	}
	
	@Test
	public void testGetVariablesNames() {
		assertEquals(asList(), aAtom.getVariablesNames());
		assertEquals(asList("A"), aVar.getVariablesNames());
		assertEquals(asList("_A"), namedAnonVar.getVariablesNames());
		assertEquals(asList("_"), ANONYMOUS_VAR.getVariablesNames());
		
		assertEquals(asList("_"), t0.getVariablesNames());
		assertEquals(asList("_", "A"), t1.getVariablesNames());
		assertEquals(asList("_", "A"), t2.getVariablesNames());
		assertEquals(asList("_", "A", "B", "_A"), t3.getVariablesNames());
	}
	
	@Test
	public void testNonAnonymousVariablesNames() {
		assertEquals(asList(), aAtom.nonAnonymousVariablesNames());
		assertEquals(asList("A"), aVar.nonAnonymousVariablesNames());
		assertEquals(asList(), namedAnonVar.nonAnonymousVariablesNames());
		assertEquals(asList(), ANONYMOUS_VAR.nonAnonymousVariablesNames());
		
		assertEquals(asList(), t0.nonAnonymousVariablesNames());
		assertEquals(asList("A"), t1.nonAnonymousVariablesNames());
		assertEquals(asList("A"), t2.nonAnonymousVariablesNames());
		assertEquals(asList("A", "B"), t3.nonAnonymousVariablesNames());
	}
	
	@Test
	public void testChangeVariablesNames() {
		Variable newAVar = new Variable("NewA");
		Variable newAnon = new Variable("ANONYMOUS");
		Map<String, String> map = new HashMap<String, String>(){{
			put("_", "ANONYMOUS");
			put("A", "NewA");
		}};
		
		assertEquals(aAtom, aAtom.changeVariablesNames(map));
		assertEquals(newAVar, aVar.changeVariablesNames(map));
		assertEquals(newAnon, ANONYMOUS_VAR.changeVariablesNames(map));
		
		assertEquals(new Compound(aAtom, asList(newAnon)), 
				t0.changeVariablesNames(map));
		assertEquals(new Compound(aAtom, asList(new Compound(newAnon, asList(newAVar)))), 
				t1.changeVariablesNames(map));
		assertEquals(new Compound(aAtom, asList(new Compound(newAnon, asList(new ListTerm(asList(newAVar, newAVar)))))), 
				t2.changeVariablesNames(map));
		assertEquals(new Compound(aAtom, asList(new Compound(newAnon, asList(new ListTerm(asList(aAtom, newAVar, bVar, namedAnonVar, newAnon)))))), 
				t3.changeVariablesNames(map));
	}
	
	@Test
	public void testReplaceVariables() {
		final Variable newAVar = new Variable("NewA");
		final Variable newAnon = new Variable("ANONYMOUS");
		Map<String, TermConvertable> map = new HashMap<String, TermConvertable>(){{
			put("_", newAnon);
			put("A", newAVar);
		}};
		
		assertEquals(aAtom, aAtom.replaceVariables(map));
		assertEquals(newAVar, aVar.replaceVariables(map));
		assertEquals(newAnon, ANONYMOUS_VAR.replaceVariables(map));
		
		assertEquals(new Compound(aAtom, asList(newAnon)), 
				t0.replaceVariables(map));
		assertEquals(new Compound(aAtom, asList(new Compound(newAnon, asList(newAVar)))), 
				t1.replaceVariables(map));
		assertEquals(new Compound(aAtom, asList(new Compound(newAnon, asList(new ListTerm(asList(newAVar, newAVar)))))), 
				t2.replaceVariables(map));
		assertEquals(new Compound(aAtom, asList(new Compound(newAnon, asList(new ListTerm(asList(aAtom, newAVar, bVar, namedAnonVar, newAnon)))))), 
				t3.replaceVariables(map));
	}
	
	

	
}
