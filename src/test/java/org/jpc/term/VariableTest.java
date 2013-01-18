package org.jpc.term;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import org.jpc.converter.toterm.DefaultToTermConverter;
import org.jpc.term.Variable;
import org.junit.Test;

public class VariableTest {

	@Test
	public void testVariableInstantiation() {
		new Variable("VarName"); //fine
		new Variable("_varName"); //fine
		new Variable("__VarName"); //fine
		new Variable("_"); //fine
		try {
			new Variable("varName"); //wrong, variables should start with capital letters or _
			fail();
		} catch(Exception e){}//expected
		try {
			new Variable("1varName"); //wrong, variables should start with capital letters or _
			fail();
		} catch(Exception e){}//expected
	}
	
	@Test
	public void testEquality() {
		assertEquals(new Variable("A"), new Variable("A"));
		assertEquals(new Variable("A").hashCode(), new Variable("A").hashCode());
		assertEquals(new Variable("_A"), new Variable("_A"));
		assertFalse(new Variable("A").equals(new Variable("_A")));
		assertFalse(new Variable("A").equals(new Variable("B")));
		assertFalse(new Variable("A").equals(new Variable("_")));
		assertFalse(new Variable("_").equals(new Variable("A")));
		assertFalse(new Variable("_").equals(new Variable("_")));
		assertTrue(new Variable("_").termEquals(new Variable("_")));
	}
	
	@Test
	public void testArity() {
		assertEquals(new Variable("A").arity(), 0);
		assertEquals(new Variable("_").arity(), 0);
	}
	
	@Test
	public void testHasFunctor() {
		assertTrue(new Variable("A").hasFunctor(new Variable("A"), 0));
		assertTrue(new Variable("_A").hasFunctor(new Variable("_A"), 0));
		assertTrue(new Variable("_").hasFunctor(new Variable("_"), 0));
	}
	
}
