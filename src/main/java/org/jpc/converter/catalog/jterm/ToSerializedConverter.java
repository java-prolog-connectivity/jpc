package org.jpc.converter.catalog.jterm;

import java.io.Serializable;

import org.jpc.Jpc;
import org.jpc.converter.ToTermConverter;
import org.jpc.term.Compound;
import org.jpc.term.jterm.Serialized;

public class ToSerializedConverter implements ToTermConverter<Serializable, Compound> {

	@Override
	public Compound toTerm(Serializable object, Class<Compound> termClass, Jpc context) {
		return Serialized.jSerializedTerm(object);
	}

}
