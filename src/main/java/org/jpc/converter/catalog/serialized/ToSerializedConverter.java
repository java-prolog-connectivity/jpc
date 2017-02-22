package org.jpc.converter.catalog.serialized;

import java.io.Serializable;

import org.jconverter.converter.TypeDomain;
import org.jpc.Jpc;
import org.jpc.converter.ToTermConverter;
import org.jpc.term.Compound;
import org.jpc.term.SerializedTerm;

public class ToSerializedConverter implements ToTermConverter<Serializable, Compound> {

	@Override
	public Compound toTerm(Serializable object, TypeDomain target, Jpc context) {
		return SerializedTerm.serialize(object);
	}

}
