package org.jpc.converter.fromterm;

import java.lang.reflect.Type;
import java.util.Calendar;

import org.jpc.Jpc;
import org.jpc.converter.JpcConversionException;
import org.jpc.term.Term;

public class TermToCalendarConverter extends FromTermConverter<Calendar> {

	@Override
	public Calendar convert(Term term, Type type, Jpc context) {
		Calendar calendar;
		long timeInMilliSeconds;
		
		try {
			calendar = context.instantiate(type);
			timeInMilliSeconds = context.fromTerm(term, Long.class);
		} catch(Exception e) {
			throw new JpcConversionException();
		}

		calendar.setTimeInMillis(timeInMilliSeconds);
		return calendar;
	}

}
