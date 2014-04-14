package org.jpc.converter.catalog.error;

import static java.util.Arrays.asList;

import org.jpc.Jpc;
import org.jpc.converter.ToTermConverter;
import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.Term;
import org.jpc.term.Var;

public class JExceptionConverter implements ToTermConverter<Throwable, Compound> {

	public static final String JEXCEPTION_FUNCTOR_NAME = "jexception";
	
	@Override
	public Compound toTerm(Throwable ex, Class<Compound> termClass, Jpc jpc) {
		Term stackTraceTerm = jpc.toTerm(ex.getStackTrace());
		Term causeTerm = jpc.toTerm(ex.getCause());
		Term messageTerm = ex.getMessage() != null ? new Atom(ex.getMessage()) : Var.ANONYMOUS_VAR;
		return new Compound(JEXCEPTION_FUNCTOR_NAME, asList(messageTerm, stackTraceTerm, causeTerm));
	}

}
