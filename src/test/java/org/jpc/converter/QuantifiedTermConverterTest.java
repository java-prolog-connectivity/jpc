package org.jpc.converter;

import static java.util.Arrays.asList;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.lang.reflect.Type;

import org.jconverter.converter.ConversionException;
import org.jpc.Jpc;
import org.jpc.JpcBuilder;
import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.Var;
import org.junit.Test;

public class QuantifiedTermConverterTest {

	class HelloConverter implements FromTermConverter<Compound, String> {
		@Override
		public String fromTerm(Compound term, Type targetType, Jpc context) {
			return term.getNameString() + " " + ((Atom)term.arg(1)).getName();
		}
	}
	
	@Test
	public void testNonGroundQuantification() {
		JpcBuilder builder = JpcBuilder.create();
		Compound helloCompound = new Compound("hello",  asList(Var.ANONYMOUS_VAR)); //the converter will be applied for any term having the form: hello(_)
		builder.register(helloCompound, new HelloConverter());
		Jpc jpc = builder.build();
		Compound helloWorldCompound = new Compound("hello",  asList(new Atom("world")));
		String s = jpc.fromTerm(helloWorldCompound);
		assertEquals("hello world", s);
		
		helloWorldCompound = new Compound("hellox",  asList(new Atom("world")));
		try {
			jpc.fromTerm(helloWorldCompound);
			fail();
		} catch(ConversionException e) {}
	}
	
	@Test
	public void testGroundQuantification() {
		JpcBuilder builder = JpcBuilder.create();
		Compound helloCompound = new Compound("hello",  asList(new Atom("world"))); //the converter will be applied for any term having the form: hello(world)
		builder.register(helloCompound, new HelloConverter());
		Jpc jpc = builder.build();
		Compound helloWorldCompound = new Compound("hello",  asList(new Atom("world")));
		String s = jpc.fromTerm(helloWorldCompound);
		assertEquals("hello world", s);
		
		helloWorldCompound = new Compound("hello",  asList(new Atom("worldx")));
		try {
			jpc.fromTerm(helloWorldCompound);
			fail();
		} catch(ConversionException e) {}
		
		Compound ungroundCompound = new Compound("hello",  asList(new Var("X")));
		s = jpc.fromTerm(ungroundCompound);
		assertEquals("hello world", s);
		
		//since the argument of the compound is an anonymous variable, this variable will not be replaced from the unified term in the Prolog database (the atom "world").
		ungroundCompound = new Compound("hello",  asList(Var.ANONYMOUS_VAR)); 
		try {
			jpc.fromTerm(ungroundCompound);
			fail(); //Implementation note: still trying to decide if this is a feature or a bug.
		} catch(ClassCastException e) {} //as expected, the anonymous variable cannot be cast to the expected atom in the HelloConverter.
	}
	
}
