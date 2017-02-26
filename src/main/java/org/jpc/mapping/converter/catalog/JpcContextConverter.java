package org.jpc.mapping.converter.catalog;

import org.jconverter.converter.TypeDomain;
import org.jpc.Jpc;
import org.jpc.JpcException;
import org.jpc.JpcImpl;
import org.jpc.mapping.converter.FromTermConverter;
import org.jpc.term.Atom;
import org.jpc.term.Compound;

public class JpcContextConverter implements FromTermConverter<Compound, Jpc> {

	public static final String JPC_FUNCTOR = "jpc";
	public static final String DEFAULT_JPC_ID = "default";
	
	@Override
	public Jpc fromTerm(Compound term, TypeDomain target, Jpc context) {
		String jpcId = ((Atom)term.arg(1)).getName();
		if(!jpcId.equals(DEFAULT_JPC_ID))
			throw new JpcException("Unrecognized Jpc id: " + jpcId);
		else
			return JpcImpl.getDefault();
	}

}
