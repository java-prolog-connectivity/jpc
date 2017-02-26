package org.jpc.mapping.converter.catalog.datetime;

import java.util.Calendar;

import org.jconverter.converter.TypeDomain;
import org.jpc.Jpc;
import org.jpc.mapping.converter.FromTermConverter;
import org.jpc.mapping.converter.ToTermConverter;
import org.jpc.term.Number;

public class CalendarToNumberTermConverter<T extends Calendar, U extends Number> implements ToTermConverter<T, U>, FromTermConverter<U, T> {

	@Override
	public U toTerm(T calendar, TypeDomain target, Jpc context) {
		Object primitiveCalendar = context.convert(calendar, Long.class);  //the default context will return the time in milliseconds.
		return context.toTerm(primitiveCalendar, target);
	}


	@Override
	public T fromTerm(U term, TypeDomain target, Jpc context) {
		Object primitiveCalendar = context.fromTerm(term, Long.class);
		return context.convert(primitiveCalendar, target);
	}
	
}
