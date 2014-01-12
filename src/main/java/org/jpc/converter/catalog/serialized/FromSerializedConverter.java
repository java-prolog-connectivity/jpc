package org.jpc.converter.catalog.serialized;

import java.io.Serializable;
import java.lang.reflect.Type;

import javax.xml.bind.DatatypeConverter;

import org.jconverter.converter.ConversionException;
import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.SerializedTerm;

public class FromSerializedConverter<T extends Serializable> implements FromTermConverter<Compound,T> {

	@Override
	public T fromTerm(Compound term, Type type, Jpc context) {
		if(!term.hasFunctor(SerializedTerm.SERIALIZED_TERM_FUNCTOR, 1)) {
			throw new ConversionException();
		}
		Atom atom = (Atom) term.arg(1);
		byte[] bytes = DatatypeConverter.parseBase64Binary(atom.getName());
		return SerializedTerm.deserialize(bytes);
	}
	
}
