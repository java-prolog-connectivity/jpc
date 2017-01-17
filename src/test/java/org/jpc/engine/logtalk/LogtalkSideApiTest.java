package org.jpc.engine.logtalk;

import static org.jpc.engine.prolog.PrologEngines.defaultPrologEngine;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.jpc.engine.prolog.PrologEngine;
import org.jpc.query.Solution;
import org.jpc.term.Atom;
import org.jpc.term.Integer;
import org.jpc.term.Term;
import org.junit.Before;
import org.junit.Test;

public class LogtalkSideApiTest {

	/**
	 * Simple fixture class used for many tests.
	 * It must be public, otherwise it would not be accessible from the Prolog side.
	 * @author sergioc
	 *
	 */
	public static class Fixture1 {
		public static String x;
		public static String y;
		public static Boolean z;
		public static Map<String,String> map;
		public static List<Entry<String, java.lang.Integer>> entryList;
		
		public static long whatIsTheAnswer() {
			return 42;
		}
	}
	
	public static class Fixture2 {
		public static String x;
		public static String y;
	}

	@Before
	public void resetFixture() {
		Fixture1.entryList = null;
		Fixture1.map = null;
		Fixture1.x = null;
		Fixture1.y = null;
		Fixture1.z = null;
		
		Fixture2.x = null;
		Fixture2.y = null;
	}
	
	public static void unify(Term term1, Term term2) {
		term1.unify(term2);
	}
	
	public static void unify(Term term1, Term term2, Term term3) {
		term1.unify(term2);
		term2.unify(term3);
	}
	
	/* ********************************************************************************************************************************
	 * Testing linguistic symbiosis.
     **********************************************************************************************************************************
     */
	
	@Test
	public void testNewWithShortNotation() {
		Term term;
		//class('java.lang.String') is interpreted as the class java.lang.String
		term = defaultPrologEngine().query("class('java.lang.String')::new('hello') return term(V)").oneSolutionOrThrow().get("V");
		assertEquals(new Atom("hello"), term);
	}
	
	@Test
	public void testNew() {
		Term term;
		//class([java,lang],['String']) is interpreted as the class java.lang.String
		term = defaultPrologEngine().query("class([java,lang],['String'])::new('hello') return term(V)").oneSolutionOrThrow().get("V");
		assertEquals(new Atom("hello"), term);
	}
	
	@Test
	public void testNewWithReturnParam() {
		Term term;
		//class('java.lang.String') is interpreted as the class java.lang.String
		term = defaultPrologEngine().query("class('java.lang.String')::new('hello', return(term(V)))").oneSolutionOrThrow().get("V");
		assertEquals(new Atom("hello"), term);
	}
	
	@Test
	public void testMap() {
		Term term;
		//[a:1,b:2] is interpreted as a map. It could also been written as [a-1,b-2] or [a=1,b=2].
		term = defaultPrologEngine().query("[a:1,b:2]::get('a') return term(X)").oneSolutionOrThrow().get("X");
		assertEquals(new Integer(1), term);
	}
	
	@Test
	public void testIndexedValues() {
		Term term;
		//[a,b,c] is interpreted as a list.
		term = defaultPrologEngine().query("[a,b,c]::[1] return term(X)").oneSolutionOrThrow().get("X");
		assertEquals(new Atom("b"), term);
		term = defaultPrologEngine().query("[1,2,3]::[0] return term(X)").oneSolutionOrThrow().get("X");
		assertEquals(new Integer(1), term);
	}
	
	@Test
	public void testSetFieldAccordingToType() {
		defaultPrologEngine().query("class([org,jpc,engine,logtalk],['LogtalkSideApiTest','Fixture1'])::[x,true]").oneSolutionOrThrow();
		defaultPrologEngine().query("class([org,jpc,engine,logtalk],['LogtalkSideApiTest','Fixture1'])::[z,true]").oneSolutionOrThrow();
		assertEquals("true", Fixture1.x);
		assertTrue(Fixture1.z);
	}
	
	@Test
	public void testSetAndGetStaticField() {
		defaultPrologEngine().query("class([org,jpc,engine,logtalk],['LogtalkSideApiTest','Fixture1'])::[x,hello]").oneSolutionOrThrow();
		defaultPrologEngine().query("class([org,jpc,engine,logtalk],['LogtalkSideApiTest','Fixture1'])::[y,bye]").oneSolutionOrThrow();
		Atom result = (Atom) defaultPrologEngine().query("class([org,jpc,engine,logtalk],['LogtalkSideApiTest','Fixture1'])::[x] return term(X)").oneSolutionOrThrow().get("X");
		assertEquals("hello", result.getName());
		result = (Atom) defaultPrologEngine().query("class([org,jpc,engine,logtalk],['LogtalkSideApiTest','Fixture1'])::[y] return term(X)").oneSolutionOrThrow().get("X");
		assertEquals("bye", result.getName());
	}
	
	@Test
	public void testSetStaticFieldFromField() {
		defaultPrologEngine().query("class([org,jpc,engine,logtalk],['LogtalkSideApiTest','Fixture1'])::[x,hello]").oneSolutionOrThrow();
		defaultPrologEngine().query("class([org,jpc,engine,logtalk],['LogtalkSideApiTest','Fixture1'])::[y,class([org,jpc,engine,logtalk],['LogtalkSideApiTest','Fixture1'])::[x]]").oneSolutionOrThrow();
		assertEquals("hello", Fixture1.x);
		assertEquals("hello", Fixture1.y);
	}

	@Test
	public void testSetFieldInferringType() {
		//the type of the field is Map<String,String>. This determines how the term is converted.
		defaultPrologEngine().query("class([org,jpc,engine,logtalk],['LogtalkSideApiTest','Fixture1'])::[map,[x=1,y=2]]").oneSolutionOrThrow();
		assertEquals("1", Fixture1.map.get("x"));
		assertEquals("2", Fixture1.map.get("y"));
		//the type of the field is Map<String,Integer>. This determines how the term is converted.
		defaultPrologEngine().query("class([org,jpc,engine,logtalk],['LogtalkSideApiTest','Fixture1'])::[entryList,[x=1,y=2]]").oneSolutionOrThrow();
		Entry<String, java.lang.Integer> entry = Fixture1.entryList.get(0);
		assertEquals("x", entry.getKey());
		assertEquals(new java.lang.Integer(1), entry.getValue());
		entry = Fixture1.entryList.get(1);
		assertEquals("y", entry.getKey());
		assertEquals(new java.lang.Integer(2), entry.getValue());
	}
	
	
	/* ********************************************************************************************************************************
	 * Testing jobject/1 and jobject/2.
     **********************************************************************************************************************************
     */
	
	/**
	 * The purpose of jobject/1 and jobject/2 is to convert to a Java representation a term that has not been declared as a symbiotic object (i.e., there is not a Logtalk object declaration importing the "jobject" category).
	 * The second (optional) argument is the return value after the evaluation of a Logtalk message interpreted in the Java side.
	 * If no second argument is provided, the return value, if needed, can also be obtained by means of: "java(object)::message return ReturnSpecifier".
	 */
	@Test
	public void testJObject1() {
		Term term = defaultPrologEngine().query("jobject(abc)::toUpperCase return term(V)").oneSolutionOrThrow().get("V");
		assertEquals(new Atom("ABC"), term);
	}
	
	@Test
	public void testJObject2() {
		Term term = defaultPrologEngine().query("jobject(abc, term(V))::toUpperCase").oneSolutionOrThrow().get("V");
		assertEquals(new Atom("ABC"), term);
		term = defaultPrologEngine().query("jobject([a:1,b:2], term(X))::get('a')").oneSolutionOrThrow().get("X");
		assertEquals(new Integer(1), term);
	}

	
	@Test
	public void testNewWithJObject1() {
		Term term;
		term = defaultPrologEngine().query("jobject(class([java,lang],['String']))::new('hello') return term(V)").oneSolutionOrThrow().get("V");
		assertEquals(new Atom("hello"), term);
	}
	
	@Test
	public void testNewWithJObject2() {
		Term term;
		term = defaultPrologEngine().query("jobject(class([java,lang],['String']), term(V))::new").oneSolutionOrThrow().get("V");
		assertEquals(new Atom(""), term);
	}
	
	/**
	 * Tests the same message sent to multiple objects in one single step.
	 */
	@Test
	public void testBroadcasting() {
		defaultPrologEngine().query("jobject((class([org,jpc,engine,logtalk],['LogtalkSideApiTest','Fixture1']), "
				+ "class([org,jpc,engine,logtalk],['LogtalkSideApiTest','Fixture2'])))"
				+ "::[x,hello]").oneSolutionOrThrow();
		assertEquals("hello", Fixture1.x);
		assertEquals("hello", Fixture2.x);
	}
	
	
	/* ********************************************************************************************************************************
	 * Testing robject/1 and robject/2.
     **********************************************************************************************************************************
     */
	
	@Test
	public void testRObject1Invoke() {
		Term term = defaultPrologEngine().query("robject(abc)::invoke(toUpperCase) return term(V)").oneSolutionOrThrow().get("V");
		assertEquals(new Atom("ABC"), term);
	}
	
	@Test
	public void testRObject2Invoke() {
		Term term = defaultPrologEngine().query("robject(abc, term(V))::invoke(toUpperCase)").oneSolutionOrThrow().get("V");
		assertEquals(new Atom("ABC"), term);
		term = defaultPrologEngine().query("robject([a:1,b:2], term(X))::invoke(get('a'))").oneSolutionOrThrow().get("X");
		assertEquals(new Integer(1), term);
	}
	
	@Test
	public void testRObjectSetAndGetStaticField() {
		defaultPrologEngine().query("robject(class([org,jpc,engine,logtalk],['LogtalkSideApiTest','Fixture1']))::set(x,hello)").oneSolutionOrThrow();
		defaultPrologEngine().query("robject(class([org,jpc,engine,logtalk],['LogtalkSideApiTest','Fixture1']))::set(y,bye)").oneSolutionOrThrow();
		Atom result = (Atom) defaultPrologEngine().query("robject(class([org,jpc,engine,logtalk],['LogtalkSideApiTest','Fixture1']))::get(x) return term(X)").oneSolutionOrThrow().get("X");
		assertEquals("hello", result.getName());
		result = (Atom) defaultPrologEngine().query("robject(class([org,jpc,engine,logtalk],['LogtalkSideApiTest','Fixture1']),term(X))::get(y)").oneSolutionOrThrow().get("X");
		assertEquals("bye", result.getName());
	}
	
	
	/* ********************************************************************************************************************************
	 * Testing java/0
     **********************************************************************************************************************************
     */
	
	
	@Test
	public void testInvoke() {
		Term term = defaultPrologEngine().query("java::invoke([a:1,b:2], get('a'), term(X))").oneSolutionOrThrow().get("X");
		assertEquals(new Integer(1), term);
	}
	
	@Test
	public void testSetAndGetField() {
		defaultPrologEngine().query("java::set_field(class([org,jpc,engine,logtalk],['LogtalkSideApiTest','Fixture1']),x,'hello')").oneSolutionOrThrow();
		Atom result = (Atom) defaultPrologEngine().query("java::get_field(class([org,jpc,engine,logtalk],['LogtalkSideApiTest','Fixture1']),x,term(X))").oneSolutionOrThrow().get("X");
		assertEquals("hello", result.getName());
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
		//setting the static fields of a class.
		defaultPrologEngine().query("java::eval(class([org,jpc,engine,logtalk],['LogtalkSideApiTest','Fixture1'])::([x,hello]))").oneSolutionOrThrow();
		defaultPrologEngine().query("java::eval(class([org,jpc,engine,logtalk],['LogtalkSideApiTest','Fixture1'])::([y,bye]))").oneSolutionOrThrow();
		assertEquals("hello", Fixture1.x);
		assertEquals("bye", Fixture1.y);
		Atom result = (Atom) defaultPrologEngine().query("java::eval(class([org,jpc,engine,logtalk],['LogtalkSideApiTest','Fixture1'])::[x], term(X))").oneSolutionOrThrow().get("X");
		assertEquals("hello", result.getName());
		result = (Atom) defaultPrologEngine().query("java::eval(class([org,jpc,engine,logtalk],['LogtalkSideApiTest','Fixture1'])::[y], term(Y))").oneSolutionOrThrow().get("Y");
		assertEquals("bye", result.getName());
	}
	
	/**
	 * Test the evaluation of a sequence of expressions.
	 */
	@Test
	public void testEvalSequence() {
		//setting two static fields in a class as a sequence of messages.
		defaultPrologEngine().query("java::eval(("
				+ "class([org,jpc,engine,logtalk],['LogtalkSideApiTest','Fixture1'])::[x,hello], "
				+ "class([org,jpc,engine,logtalk],['LogtalkSideApiTest','Fixture1'])::[y,bye]"
				+ "))").oneSolutionOrThrow();
		assertEquals("hello", Fixture1.x);
		assertEquals("bye", Fixture1.y);
	}
	
	/**
	 * Test the return value in a sequence of expressions.
	 * The return value in a sequence is considered the value of the last expressions.
	 */
	@Test
	public void testEvalSequenceWithReturnExpression() {
		Atom result = (Atom) defaultPrologEngine().query("java::eval(("
				+ "class([org,jpc,engine,logtalk],['LogtalkSideApiTest','Fixture1'])::[x,hello], "
				+ "class([org,jpc,engine,logtalk],['LogtalkSideApiTest','Fixture1'])::[y,bye], "
				+ "class([org,jpc,engine,logtalk],['LogtalkSideApiTest','Fixture1'])::[y]"
				+ "), term(X))").oneSolutionOrThrow().get("X");
		assertEquals("bye", result.getName());
		assertEquals("hello", Fixture1.x);
		assertEquals("bye", Fixture1.y);
	}
	
	/**
	 * Tests various messages sent to the same object.
	 */
	@Test
	public void testCascading() {
		defaultPrologEngine().query("java::eval("
				+ "class([org,jpc,engine,logtalk],['LogtalkSideApiTest','Fixture1'])::"
				+ "([x,hello],[y,bye]))"
				).oneSolutionOrThrow();
		assertEquals("hello", Fixture1.x);
		assertEquals("bye", Fixture1.y);
	}
	
	/**
	 * Tests various messages sent to various objects.
	 */
	@Test
	public void testCascadingAndBroadcasting() {
		defaultPrologEngine().query("java::eval("
				+ "(class([org,jpc,engine,logtalk],['LogtalkSideApiTest','Fixture1']), "
				+ "class([org,jpc,engine,logtalk],['LogtalkSideApiTest','Fixture2']))::"
				+ "([x,hello],[y,bye]))"
				).oneSolutionOrThrow();
		assertEquals("hello", Fixture1.x);
		assertEquals("bye", Fixture1.y);
		assertEquals("hello", Fixture2.x);
		assertEquals("bye", Fixture2.y);
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
	
	@Test
	public void testSimpleUnify() {
		Term term = defaultPrologEngine().query("class([org,jpc,engine,logtalk],['LogtalkSideApiTest'])::unify(term(x), term(V))").oneSolutionOrThrow().get("V");
		assertEquals(new Atom("x"), term);
		term = defaultPrologEngine().query("class([org,jpc,engine,logtalk],['LogtalkSideApiTest'])::unify(term(V), term(x))").oneSolutionOrThrow().get("V");
		assertEquals(new Atom("x"), term);
	}
	
	@Test
	public void testUnifyThreeVars() {
		Term term;
		Solution solution;
		solution = defaultPrologEngine().query("class([org,jpc,engine,logtalk],['LogtalkSideApiTest'])::unify(term(x), term(V), term(W))").oneSolutionOrThrow();
		term = solution.get("V");
		assertEquals(new Atom("x"), term);
		term = solution.get("W");
		assertEquals(new Atom("x"), term);
		
		solution = defaultPrologEngine().query("class([org,jpc,engine,logtalk],['LogtalkSideApiTest'])::unify(term(V), term(x), term(W))").oneSolutionOrThrow();
		term = solution.get("V");
		assertEquals(new Atom("x"), term);
		term = solution.get("W");
		assertEquals(new Atom("x"), term);
		
		solution = defaultPrologEngine().query("class([org,jpc,engine,logtalk],['LogtalkSideApiTest'])::unify(term(V), term(W), term(x))").oneSolutionOrThrow();
		term = solution.get("V");
		assertEquals(new Atom("x"), term);
		term = solution.get("W");
		assertEquals(new Atom("x"), term);
		
		solution = defaultPrologEngine().query("class([org,jpc,engine,logtalk],['LogtalkSideApiTest'])::unify(term(x), term(W), term(x))").oneSolutionOrThrow();
		term = solution.get("W");
		assertEquals(new Atom("x"), term);
	}
	
	
	
}
