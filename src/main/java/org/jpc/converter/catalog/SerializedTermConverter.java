package org.jpc.converter.catalog;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.ObjectInput;
import java.io.ObjectInputStream;
import java.lang.reflect.Type;

import javax.xml.bind.DatatypeConverter;

import org.jpc.Jpc;
import org.jpc.converter.JpcConversionException;
import org.jpc.converter.JpcConverter;
import org.jpc.jterm.SerializedObject;
import org.jpc.term.Atom;
import org.jpc.term.Compound;

public class SerializedTermConverter extends JpcConverter<Object, Compound> {

	@Override
	public Object fromTerm(Compound term, Type type, Jpc context) {
		if(!term.hasFunctor(SerializedObject.SERIALIZED_TERM_FUNCTOR, 1)) {
			throw new JpcConversionException();
		}
		Atom atom = (Atom) term.arg(1);
		byte[] bytes = DatatypeConverter.parseBase64Binary(atom.getName());

		try(ByteArrayInputStream bis = new ByteArrayInputStream(bytes);
				ObjectInput in = new ObjectInputStream(bis)) {
			return in.readObject();
		} catch (IOException | ClassNotFoundException e) {
			throw new RuntimeException(e);
		}
	}
	
}
