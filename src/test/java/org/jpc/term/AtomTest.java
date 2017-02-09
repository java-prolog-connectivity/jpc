package org.jpc.term;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.jpc.engine.dialect.Dialect;
import org.junit.Test;

public class AtomTest {

	@Test
	public void testEquality() {
		assertEquals(new Atom("hello"), new Atom("hello"));
		assertEquals(new Atom("hello").hashCode(), new Atom("hello").hashCode());
		assertFalse(new Atom("hello").equals(new Var("_")));
		assertFalse(new Atom("Hello").equals(new Var("Hello")));
	}
	
	@Test
	public void testArity() {
		assertEquals(new Atom("hello").arity(), 0);
	}
	
	@Test
	public void testHasFunctor() {
		assertTrue(new Atom("hello").hasFunctor(new Functor(new Atom("hello"), 0)));
	}
	
	@Test
	public void testToEscapedString() {
		assertEquals("''", new Atom("").toEscapedString(Dialect.JPC));
		assertEquals("hello", new Atom("hello").toEscapedString(Dialect.JPC));
		assertEquals("hello_world", new Atom("hello_world").toEscapedString(Dialect.JPC));
		assertEquals("hello_world123", new Atom("hello_world123").toEscapedString(Dialect.JPC));
		assertEquals("'123hello_world'", new Atom("123hello_world").toEscapedString(Dialect.JPC));
		assertEquals("'_hello_world'", new Atom("_hello_world").toEscapedString(Dialect.JPC));
		assertEquals("'Hello_world'", new Atom("Hello_world").toEscapedString(Dialect.JPC));
		assertEquals("'hello world'", new Atom("hello world").toEscapedString(Dialect.JPC));
		assertEquals("'hello ''world'''", new Atom("hello 'world'").toEscapedString(Dialect.JPC));
		assertEquals("'hello\\\\world'", new Atom("hello\\world").toEscapedString(Dialect.JPC));
		assertEquals("'hello\\\\''world'", new Atom("hello\\'world").toEscapedString(Dialect.JPC));
	}

}
