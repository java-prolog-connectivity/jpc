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

public class CustomConverterTest {

	class HelloConverter implements FromTermConverter<Compound, String> {
		@Override
		public String fromTerm(Compound term, Type targetType, Jpc context) {
			return term.getNameString() + " " + ((Atom)term.arg(1)).getName();
		}
	}
	
	@Test
	public void testQueriedTermConverter() {
		JpcBuilder builder = JpcBuilder.create();
		Compound helloCompound = new Compound("hello",  asList(Var.ANONYMOUS_VAR));
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
	
}
