package org.jpc.converter.fromterm;

import java.lang.reflect.Type;
import java.util.Calendar;

import org.jpc.Jpc;
import org.jpc.term.Term;

public class TermToCalendarConverter extends FromTermConverter<Calendar> {

	@Override
	public Calendar convert(Term term, Type type, Jpc context) {
		long timeInMilliSeconds = context.fromTerm(term, Long.class);
		Calendar calendar = context.instantiate(type);
		calendar.setTimeInMillis(timeInMilliSeconds);
		return calendar;
	}

}
