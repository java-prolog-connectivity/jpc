package org.jpc.converter.catalog.jterm;

import java.lang.reflect.Type;

import javax.xml.bind.DatatypeConverter;

import org.jconverter.converter.ConversionException;
import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.jterm.Serialized;

public class SerializedConverter<T> implements FromTermConverter<Compound,T> {

	@Override
	public T fromTerm(Compound term, Type type, Jpc context) {
		if(!term.hasFunctor(Serialized.SERIALIZED_TERM_FUNCTOR, 1)) {
			throw new ConversionException();
		}
		Atom atom = (Atom) term.arg(1);
		byte[] bytes = DatatypeConverter.parseBase64Binary(atom.getName());
		return Serialized.deserialize(bytes);
	}
	
}
