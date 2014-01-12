package org.jpc.converter.catalog.serialized;

import java.io.Serializable;

import org.jpc.Jpc;
import org.jpc.converter.ToTermConverter;
import org.jpc.term.Compound;
import org.jpc.term.SerializedTerm;

public class ToSerializedConverter implements ToTermConverter<Serializable, Compound> {

	@Override
	public Compound toTerm(Serializable object, Class<Compound> termClass, Jpc context) {
		return SerializedTerm.serialize(object);
	}

}
