package org.jpc.converter.catalog;

import java.lang.reflect.Type;

import javax.xml.bind.DatatypeConverter;

import org.jpc.Jpc;
import org.jpc.converter.JpcConversionException;
import org.jpc.converter.JpcConverter;
import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.jterm.Serialized;

public class SerializedConverter extends JpcConverter<Object, Compound> {

	@Override
	public Object fromTerm(Compound term, Type type, Jpc context) {
		if(!term.hasFunctor(Serialized.SERIALIZED_TERM_FUNCTOR, 1)) {
			throw new JpcConversionException();
		}
		Atom atom = (Atom) term.arg(1);
		byte[] bytes = DatatypeConverter.parseBase64Binary(atom.getName());
		return Serialized.deserialize(bytes);
	}
	
}
