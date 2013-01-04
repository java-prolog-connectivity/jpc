package org.jpc.converter.fromterm;

import java.util.GregorianCalendar;

import org.jpc.term.FloatTerm;
import org.jpc.term.Term;

public class TermToGregorianCalendarConverter implements TermToObjectConverter<GregorianCalendar> {

	@Override
	public GregorianCalendar apply(Term term) {
		FloatTerm floatTerm = (FloatTerm) term;
		long timeInMilliSeconds = floatTerm.longValue(); //time in milli-seconds
		GregorianCalendar gregorianCalendar = new GregorianCalendar();
		gregorianCalendar.setTimeInMillis(timeInMilliSeconds);
		return gregorianCalendar;
	}

}
