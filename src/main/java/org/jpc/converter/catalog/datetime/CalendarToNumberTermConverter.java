package org.jpc.converter.catalog.datetime;

import java.lang.reflect.Type;
import java.util.Calendar;

import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.converter.ToTermConverter;
import org.jpc.term.Number;

public class CalendarToNumberTermConverter<T extends Calendar, U extends Number> implements ToTermConverter<T, U>, FromTermConverter<U, T> {

	@Override
	public U toTerm(T calendar, Class<U> termClass, Jpc context) {
		Object primitiveCalendar = context.convert(calendar, Long.class);  //the default context will return the time in milliseconds.
		return context.toTerm(primitiveCalendar, termClass);
	}


	@Override
	public T fromTerm(U term, Type targetType, Jpc context) {
		Object primitiveCalendar = context.fromTerm(term, Long.class);
		return context.convert(primitiveCalendar, targetType);
	}
	
}
