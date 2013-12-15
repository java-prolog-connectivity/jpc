package org.jpc.converter.catalog.jterm;

import org.jpc.Jpc;
import org.jpc.converter.ToTermConverter;
import org.jpc.term.Compound;

public class ToJTermTermConverter implements ToTermConverter<Object, Compound> {

	@Override
	public Compound toTerm(Object object, Class<Compound> termClass, Jpc context) {
		Compound compound = context.toTerm(object, Compound.class); //only compounds should be associated with jterms.
		//Associating object reference to compound.
		//(Or verifying that the reference corresponds to an already registered object associated with this compound).
		context.getJTermManager().jTerm(compound, object); 
		return compound;
	}

}
