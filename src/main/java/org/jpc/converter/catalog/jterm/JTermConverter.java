package org.jpc.converter.catalog.jterm;

import java.lang.reflect.Type;

import org.jconverter.converter.ConversionException;
import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.term.Compound;
import org.jpc.term.jterm.JTermManager;
import org.jpc.term.jterm.JTermUtil;

public class JTermConverter<T> implements FromTermConverter<Compound, T> {

	@Override
	public T fromTerm(Compound term, Type type, Jpc context) {
		JTermManager jTermManager = context.getJTermManager();
		T resolved = (T) jTermManager.resolve(term); //try with the context jTerm manager
		if(resolved == null)
			resolved = (T) JTermUtil.getJTermManager().resolve(term); //try with the default jTerm manager
		if(resolved == null)
			throw new ConversionException();
		return resolved;
	}

}
