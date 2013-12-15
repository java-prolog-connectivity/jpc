package org.jpc.converter.catalog.datetime;

import java.lang.reflect.Type;
import java.util.Calendar;

import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.converter.ToTermConverter;
import org.jpc.term.Atom;

public class CalendarToAtomConverter<T extends Calendar> implements ToTermConverter<T, Atom>, FromTermConverter<Atom, T> {

	@Override
	public Atom toTerm(T calendar, Class<Atom> termClass, Jpc context) {
		Object primitiveCalendar = context.convert(calendar, String.class);  //according to how the context is configured, this can be the string representation of the calendar or something else
		return context.toTerm(primitiveCalendar, termClass);
	}


	@Override
	public T fromTerm(Atom term, Type targetType, Jpc context) {
		Object primitiveCalendar = context.fromTerm(term, String.class);
		return context.convert(primitiveCalendar, targetType);
	}

}
