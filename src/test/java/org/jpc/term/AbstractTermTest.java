package org.jpc.term;

import static java.util.Arrays.asList;
import static org.jpc.term.ListTerm.listTerm;
import static org.jpc.term.Var.ANONYMOUS_VAR;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import org.jpc.converter.TermConvertable;
import org.junit.Test;

/**
 * Testing the methods that query/transform terms using visitors
 * @author sergioc
 *
 */
public class AbstractTermTest {
	Term aAtom = new Atom("A");
	Term aVar = new Var("A");
	Term bVar = new Var("B");
	Term namedAnonVar = new Var("_A");

	Term t0 = new Compound(aAtom, asList(ANONYMOUS_VAR));
	Term t1 = new Compound(aAtom, asList(new Compound(ANONYMOUS_VAR, asList(aVar))));
	Term t2 = new Compound(aAtom, asList(new Compound(ANONYMOUS_VAR, asList(
			listTerm(asList(aVar, aVar))))));
	Term t3 = new Compound(aAtom, asList(new Compound(ANONYMOUS_VAR, asList(
			listTerm(asList(aAtom, aVar, bVar, namedAnonVar, ANONYMOUS_VAR))))));
	
	
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
		assertEquals(asList(), aAtom.getNamedVariablesNames());
		assertEquals(asList("A"), aVar.getNamedVariablesNames());
		assertEquals(asList("_A"), namedAnonVar.getNamedVariablesNames());
		assertEquals(asList(), ANONYMOUS_VAR.getNamedVariablesNames());
		
		assertEquals(asList(), t0.getNamedVariablesNames());
		assertEquals(asList("A"), t1.getNamedVariablesNames());
		assertEquals(asList("A"), t2.getNamedVariablesNames());
		assertEquals(asList("A", "B", "_A"), t3.getNamedVariablesNames());
	}
	
	@Test
	public void testChangeVariablesNames() {
		Var newAVar = new Var("NewA");
		Var newAnon = new Var("ANONYMOUS");
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
		assertEquals(new Compound(aAtom, asList(new Compound(newAnon, asList(listTerm(asList(newAVar, newAVar)))))), 
				t2.changeVariablesNames(map));
		assertEquals(new Compound(aAtom, asList(new Compound(newAnon, asList(listTerm(asList(aAtom, newAVar, bVar, namedAnonVar, newAnon)))))), 
				t3.changeVariablesNames(map));
	}
	
	@Test
	public void testReplaceVariables() {
		final Var newAVar = new Var("NewA");
		final Var newAnon = new Var("ANONYMOUS");
		Map<String, Term> map = new HashMap<String, Term>(){{
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
		assertEquals(new Compound(aAtom, asList(new Compound(newAnon, asList(listTerm(asList(newAVar, newAVar)))))), 
				t2.replaceVariables(map));
		assertEquals(new Compound(aAtom, asList(new Compound(newAnon, asList(listTerm(asList(aAtom, newAVar, bVar, namedAnonVar, newAnon)))))), 
				t3.replaceVariables(map));
	}
	
	

	
}
