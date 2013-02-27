package org.jpc.converter.toterm;

import java.util.Calendar;

import org.jpc.Jpc;
import org.jpc.term.IntegerTerm;
import org.jpc.term.Term;

public class CalendarToTermConverter extends ToTermConverter<Calendar> {

	@Override
	public Term convert(Calendar calendar, Jpc context) {
		long timeInMilliSeconds = calendar.getTimeInMillis();
		return new IntegerTerm(timeInMilliSeconds);
	}

}
