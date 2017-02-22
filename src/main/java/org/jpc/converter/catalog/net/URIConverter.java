package org.jpc.converter.catalog.net;

import static java.util.Arrays.asList;

import java.net.URI;
import java.net.URISyntaxException;

import org.jconverter.converter.TypeDomain;
import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.converter.ToTermConverter;
import org.jpc.term.Atom;
import org.jpc.term.Compound;

public class URIConverter implements ToTermConverter<URI, Compound>, FromTermConverter<Compound, URI> {

	public static final String URI_FUNCTOR_NAME = "uri";
	
	@Override
	public URI fromTerm(Compound compound, TypeDomain target, Jpc context) {
		try {
			return new URI(((Atom)compound.arg(1)).getName());
		} catch (URISyntaxException e) {
			throw new RuntimeException(e);
		}
	}

	@Override
	public Compound toTerm(URI uri, TypeDomain target, Jpc context) {
		return new Compound(URI_FUNCTOR_NAME, asList(new Atom(uri.toString())));
	}

}
