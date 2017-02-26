package org.jpc.mapping.converter.catalog.datetime;

import java.util.Calendar;

import org.jconverter.converter.TypeDomain;
import org.jpc.Jpc;
import org.jpc.mapping.converter.FromTermConverter;
import org.jpc.mapping.converter.ToTermConverter;
import org.jpc.term.Atom;

public class CalendarToAtomConverter<T extends Calendar> implements ToTermConverter<T, Atom>, FromTermConverter<Atom, T> {

	@Override
	public Atom toTerm(T calendar, TypeDomain target, Jpc context) {
		Object primitiveCalendar = context.convert(calendar, String.class);  //according to how the context is configured, this can be the string representation of the calendar or something else
		return context.toTerm(primitiveCalendar, target);
	}


	@Override
	public T fromTerm(Atom term, TypeDomain target, Jpc context) {
		Object primitiveCalendar = context.fromTerm(term, String.class);
		return context.convert(primitiveCalendar, target);
	}

}
