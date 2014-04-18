package org.jpc.engine.logtalk;

import static org.jpc.engine.prolog.PrologEngines.defaultPrologEngine;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import org.jpc.engine.prolog.PrologEngine;
import org.jpc.term.Atom;
import org.jpc.term.IntegerTerm;
import org.jpc.term.Term;
import org.junit.Test;

public class LogtalkSideApiTest {

	/**
	 * Simple fixture class used for many tests.
	 * It must be public, otherwise it would not be accessible from the Prolog side.
	 * @author sergioc
	 *
	 */
	public static class Fixture {
		public static String x;
		public static String y;
	}

	
	/* ********************************************************************************************************************************
	 * Testing linguistic symbiosis.
     **********************************************************************************************************************************
     */
	
	@Test
	public void testReturns() {
		Term term;
		//[a:1,b:2] is interpreted as a map. It could also been written as [a-1,b-2] or [a=1,b=2].
		term = defaultPrologEngine().query("[a:1,b:2]::get('a') returns term(X)").oneSolutionOrThrow().get("X");
		assertEquals(new IntegerTerm(1), term);
	}
	
	@Test
	public void testIndexedValues() {
		Term term;
		//[a,b,c] is interpreted as a list.
		term = defaultPrologEngine().query("[a,b,c]::[1] returns term(X)").oneSolutionOrThrow().get("X");
		assertEquals(new Atom("b"), term);
		term = defaultPrologEngine().query("[1,2,3]::[0] returns term(X)").oneSolutionOrThrow().get("X");
		assertEquals(new IntegerTerm(1), term);
	}
	
	@Test
	public void testNew() {
		Term term;
		//class([java,lang],['String']) is interpreted as the class java.lang.String
		term = defaultPrologEngine().query("class([java,lang],['String'])::new('hello') returns term(V)").oneSolutionOrThrow().get("V");
		assertEquals(new Atom("hello"), term);
	}
	
	@Test
	public void testSetStaticField() {
		Fixture.x = null;
		Fixture.y = null;
		defaultPrologEngine().query("class([org,jpc,engine,logtalk],['LogtalkSideApiTest','Fixture'])::[x,hello]").oneSolutionOrThrow();
		defaultPrologEngine().query("class([org,jpc,engine,logtalk],['LogtalkSideApiTest','Fixture'])::[y,bye]").oneSolutionOrThrow();
		Atom result = (Atom) defaultPrologEngine().query("class([org,jpc,engine,logtalk],['LogtalkSideApiTest','Fixture'])::[x] returns term(X)").oneSolutionOrThrow().get("X");
		assertEquals("hello", result.getName());
		result = (Atom) defaultPrologEngine().query("class([org,jpc,engine,logtalk],['LogtalkSideApiTest','Fixture'])::[y] returns term(X)").oneSolutionOrThrow().get("X");
		assertEquals("bye", result.getName());
	}
	
	
	/* ********************************************************************************************************************************
	 * Testing java/1 and java/2.
     **********************************************************************************************************************************
     */
	
	/**
	 * The purpose of java/1 and java/2 is to convert to a Java representation a term that has not been declared as a symbiotic object (i.e., there is not a Logtalk object declaration importing the "java_bridge" category).
	 * The second (optional) argument is the return value after the evaluation of a Logtalk message interpreted in the Java side.
	 * If no second argument is provided, the return value, if needed, can also be obtained by means of: "java(object)::message returns ReturnSpecifier".
	 */
	@Test
	public void testJava1() {
		Term term = defaultPrologEngine().query("java(abc)::toUpperCase returns term(V)").oneSolutionOrThrow().get("V");
		assertEquals(new Atom("ABC"), term);
	}
	
	@Test
	public void testJava2() {
		Term term = defaultPrologEngine().query("java(abc, term(V))::toUpperCase").oneSolutionOrThrow().get("V");
		assertEquals(new Atom("ABC"), term);
		term = defaultPrologEngine().query("java([a:1,b:2], term(X))::get('a')").oneSolutionOrThrow().get("X");
		assertEquals(new IntegerTerm(1), term);
		//the invoke method is experimental and may be deprecated.
		term = defaultPrologEngine().query("java([a:1,b:2], term(X))::invoke(get('a'))").oneSolutionOrThrow().get("X");
		assertEquals(new IntegerTerm(1), term);
	}

	
	@Test
	public void testNewWithJava1() {
		Term term;
		term = defaultPrologEngine().query("java(class([java,lang],['String']))::new('hello') returns term(V)").oneSolutionOrThrow().get("V");
		assertEquals(new Atom("hello"), term);
	}
	
	@Test
	public void testNewWithJava2() {
		Term term;
		term = defaultPrologEngine().query("java(class([java,lang],['String']), term(V))::new").oneSolutionOrThrow().get("V");
		assertEquals(new Atom(""), term);
	}
	
	
	/* ********************************************************************************************************************************
	 * Testing eval/1 and eval/2.
     **********************************************************************************************************************************
     */
	
	/**
	 * The purpose of eval/1 and eval/2 is to evaluate, in the Java side, an arbitrary Prolog term translated to a Java expression.
	 */
	@Test
	public void testEval() {
		Term term;
		//the first argument of eval is the expression to evaluate. The second (optional) argument is the return value of the expression.
		term = defaultPrologEngine().query("java::eval(abc::toUpperCase, term(V))").oneSolutionOrThrow().get("V");
		assertEquals(new Atom("ABC"), term);
	}
	
	@Test
	public void testEvalWithSideEffects() {
		Fixture.x = null;
		Fixture.y = null;
		//setting the static fields of a class.
		defaultPrologEngine().query("java::eval(class([org,jpc,engine,logtalk],['LogtalkSideApiTest','Fixture'])::([x,hello]))").oneSolutionOrThrow();
		defaultPrologEngine().query("java::eval(class([org,jpc,engine,logtalk],['LogtalkSideApiTest','Fixture'])::([y,bye]))").oneSolutionOrThrow();
		assertEquals("hello", Fixture.x);
		assertEquals("bye", Fixture.y);
		Atom result = (Atom) defaultPrologEngine().query("java::eval(class([org,jpc,engine,logtalk],['LogtalkSideApiTest','Fixture'])::[x], term(X))").oneSolutionOrThrow().get("X");
		assertEquals("hello", result.getName());
		result = (Atom) defaultPrologEngine().query("java::eval(class([org,jpc,engine,logtalk],['LogtalkSideApiTest','Fixture'])::[y], term(Y))").oneSolutionOrThrow().get("Y");
		assertEquals("bye", result.getName());
	}
	
	/**
	 * Test the evaluation of a sequence of expressions.
	 */
	@Test
	public void testEvalSequence() {
		Fixture.x = null;
		Fixture.y = null;
		//setting two static fields in a class as a sequence of messages.
		defaultPrologEngine().query("java::eval(("
				+ "class([org,jpc,engine,logtalk],['LogtalkSideApiTest','Fixture'])::[x,hello], "
				+ "class([org,jpc,engine,logtalk],['LogtalkSideApiTest','Fixture'])::[y,bye]"
				+ "))").oneSolutionOrThrow();
		assertEquals("hello", Fixture.x);
		assertEquals("bye", Fixture.y);
	}
	
	/**
	 * Test the return value in a sequence of expressions.
	 * The return value in a sequence is considered the value of the last expressions.
	 */
	@Test
	public void testEvalSequenceWithReturnExpression() {
		Fixture.x = null;
		Fixture.y = null;
		Atom result = (Atom) defaultPrologEngine().query("java::eval(("
				+ "class([org,jpc,engine,logtalk],['LogtalkSideApiTest','Fixture'])::[x,hello], "
				+ "class([org,jpc,engine,logtalk],['LogtalkSideApiTest','Fixture'])::[y,bye], "
				+ "class([org,jpc,engine,logtalk],['LogtalkSideApiTest','Fixture'])::[y]"
				+ "), term(X))").oneSolutionOrThrow().get("X");
		assertEquals("bye", result.getName());
		assertEquals("hello", Fixture.x);
		assertEquals("bye", Fixture.y);
	}
	
	
	/* ********************************************************************************************************************************
	 * Other tests.
     **********************************************************************************************************************************
     */
	
	@Test
	public void testCurrentEngine() {
		PrologEngine prologEngine = defaultPrologEngine().query("prolog_engines::this_engine(E)").<PrologEngine>selectObject("E").oneSolutionOrThrow();
		assertNotNull(prologEngine);
	}
	
}
