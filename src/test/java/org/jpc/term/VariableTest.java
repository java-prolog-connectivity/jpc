package org.jpc.term;

import static org.jpc.term.Functor.functor;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import org.junit.Test;

public class VariableTest {

	@Test
	public void testVariableInstantiation() {
		new Var("VarName"); //fine
		new Var("_varName"); //fine
		new Var("__VarName"); //fine
		new Var("_"); //fine
		try {
			new Var("varName"); //wrong, variables should start with capital letters or _
			fail();
		} catch(Exception e){}//expected
		try {
			new Var("1varName"); //wrong, variables should start with capital letters or _
			fail();
		} catch(Exception e){}//expected
	}
	
	@Test
	public void testEquality() {
		assertEquals(new Var("A"), new Var("A"));
		assertEquals(new Var("A").hashCode(), new Var("A").hashCode());
		assertEquals(new Var("_A"), new Var("_A"));
		assertFalse(new Var("A").equals(new Var("_A")));
		assertFalse(new Var("A").equals(new Var("B")));
		assertFalse(new Var("A").equals(new Var("_")));
		assertFalse(new Var("_").equals(new Var("A")));
		assertFalse(new Var("_").equals(new Var("_")));
		assertTrue(new Var("_").termEquals(new Var("_")));
	}
	
	@Test
	public void testArity() {
		assertEquals(new Var("A").arity(), 0);
		assertEquals(new Var("_").arity(), 0);
	}
	
	@Test
	public void testHasFunctor() {
		assertTrue(new Var("A").hasFunctor(functor(new Var("A"), 0)));
		assertTrue(new Var("_A").hasFunctor(functor(new Var("_A"), 0)));
		assertTrue(new Var("_").hasFunctor(functor(new Var("_"), 0)));
	}
	
}
