package org.jpc.converter;

import static java.util.Arrays.asList;
import static org.jpc.term.Var.dontCare;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import org.jconverter.converter.NotSuitableConverterException;
import org.jconverter.converter.TypeDomain;
import org.jpc.Jpc;
import org.jpc.JpcBuilder;
import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.Var;
import org.junit.Test;

public class CustomTermConversionsTest {

	class HelloConverter implements FromTermConverter<Compound, String> {
		@Override
		public String fromTerm(Compound term, TypeDomain target, Jpc context) {
			return term.getName() + " " + ((Atom) term.arg(1)).getName();
		}
	}
	
	@Test
	public void testNonGroundQuantification() {
		JpcBuilder builder = JpcBuilder.create();
		Compound helloCompound = new Compound("hello",  asList(dontCare())); //the converter will be applied for any term having the form: hello(_)
		builder.register(new HelloConverter(), helloCompound);
		Jpc jpc = builder.build();
		Compound helloWorldCompound = new Compound("hello",  asList(new Atom("world")));
		String s = jpc.fromTerm(helloWorldCompound);
		assertEquals("hello world", s);
		
		helloWorldCompound = new Compound("hellox",  asList(new Atom("world")));
		try {
			jpc.fromTerm(helloWorldCompound);
			fail();
		} catch(NotSuitableConverterException e) {}
	}
	
	@Test
	public void testGroundQuantification() {
		JpcBuilder builder = JpcBuilder.create();
		Compound helloCompound = new Compound("hello",  asList(new Atom("world"))); //the converter will be applied for any term having the form: hello(world)
		builder.register(new HelloConverter(), helloCompound);
		Jpc jpc = builder.build();
		Compound helloWorldCompound = new Compound("hello",  asList(new Atom("world")));
		String s = jpc.fromTerm(helloWorldCompound);
		assertEquals("hello world", s);
		
		helloWorldCompound = new Compound("hello",  asList(new Atom("worldx")));
		try {
			jpc.fromTerm(helloWorldCompound);
			fail();
		} catch(NotSuitableConverterException e) {}
		
		Compound ungroundCompound = new Compound("hello",  asList(new Var("X")));
		s = jpc.fromTerm(ungroundCompound);
		assertEquals("hello world", s);
		
		//since the argument of the compound is an anonymous variable, this variable will not be replaced from the unified term in the Prolog database (the atom "world").
		ungroundCompound = new Compound("hello",  asList(dontCare()));
		try {
			jpc.fromTerm(ungroundCompound);
			fail(); //Implementation note: still trying to decide if this is a feature or a bug.
		} catch(ClassCastException e) {} //as expected, the anonymous variable cannot be cast to the expected atom in the HelloConverter.
	}
	
	
	class MyClass {}
	private final String atomForMyClass = "my_class";
	
	class MyClassConverter implements FromTermConverter<Atom, MyClass>, ToTermConverter<MyClass, Atom> {

		@Override
		public Atom toTerm(MyClass object, TypeDomain target, Jpc context) {
			return new Atom(atomForMyClass);
		}

		@Override
		public MyClass fromTerm(Atom term, TypeDomain target, Jpc context) {
			return new MyClass();
		}
	}
	
	
	@Test
	public void testCustomAtomConverter() {
		JpcBuilder builder = JpcBuilder.create();
		builder.register(new MyClassConverter());
		Jpc jpc = builder.build();
		
		assertEquals(atomForMyClass, jpc.fromTerm(new Atom(atomForMyClass)));
		assertEquals(new Atom(atomForMyClass), jpc.toTerm(atomForMyClass));
		
		assertTrue(jpc.fromTerm(new Atom(atomForMyClass), MyClass.class) instanceof MyClass);
		assertEquals(new Atom(atomForMyClass), jpc.toTerm(new MyClass()));
	}
	
//	@Test
//	public void testAtomQuantifiedConverter() { //this test has been commented out since in the last JPC version atoms cannot be employed to quantify over the domain of term to object converters.
//		JpcBuilder builder = JpcBuilder.create();
//		Atom atom = new Atom(atomForMyClass);
//		builder.register(new MyClassConverter(), atom);
//		Jpc jpc = builder.build();
//		
//		assertEquals(atomForMyClass, jpc.fromTerm(new Atom(atomForMyClass)));
//		assertEquals(new Atom(atomForMyClass), jpc.toTerm(atomForMyClass));
//		
//		assertTrue(jpc.fromTerm(new Atom(atomForMyClass), MyClass.class) instanceof MyClass);
//		assertEquals(new Atom(atomForMyClass), jpc.toTerm(new MyClass()));
//	}

}
