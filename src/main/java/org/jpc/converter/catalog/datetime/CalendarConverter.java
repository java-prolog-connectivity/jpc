package org.jpc.converter.catalog.datetime;

import java.lang.reflect.Type;
import java.util.Calendar;

import org.jpc.Jpc;
import org.jpc.converter.BidirectionalTermConverter;
import org.jpc.term.Atom;
import org.jpc.term.Term;

public class CalendarConverter<T extends Calendar, U extends Term> implements BidirectionalTermConverter<T, U> {

	@Override
	public U toTerm(T calendar, Class<U> termClass, Jpc context) {
		Object primitiveCalendar;
		if(termClass.equals(Atom.class))
			primitiveCalendar = context.convert(calendar, String.class);  //according to how the context is configured, this can be the string representation of the calendar or something else
		else
			primitiveCalendar = context.convert(calendar, Long.class);  //the default context will return the time in milliseconds.
		return context.toTerm(primitiveCalendar, termClass);
	}


	@Override
	public T fromTerm(U term, Type targetType, Jpc context) {
		Object primitiveCalendar;
		if(term instanceof Atom)
			primitiveCalendar = context.fromTerm(term, String.class);
		else
			primitiveCalendar = context.fromTerm(term, Long.class);
		return context.convert(primitiveCalendar, targetType);
	}
	
}
