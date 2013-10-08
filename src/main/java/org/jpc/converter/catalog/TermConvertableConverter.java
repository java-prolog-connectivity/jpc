package org.jpc.converter.catalog;

import org.jpc.Jpc;
import org.jpc.converter.JpcConverter;
import org.jpc.converter.TermConvertable;
import org.jpc.term.Term;

public class TermConvertableConverter extends JpcConverter<TermConvertable<?>, Term> {

	@Override
	public Term toTerm(TermConvertable<?> o, Jpc context) {
		return o.asTerm();
	}

}
