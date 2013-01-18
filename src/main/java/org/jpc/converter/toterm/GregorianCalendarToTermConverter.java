package org.jpc.converter.toterm;

import java.util.GregorianCalendar;

import org.jpc.term.FloatTerm;
import org.jpc.term.Term;

public class GregorianCalendarToTermConverter implements ToTermConverter<GregorianCalendar> {

	@Override
	public Term apply(GregorianCalendar calendar) {
		long timeInMilliSeconds = calendar.getTimeInMillis();
		return new FloatTerm(timeInMilliSeconds);
	}

}
