package org.jpc.converter.catalog;

import java.lang.reflect.Type;
import java.util.Calendar;

import org.jpc.Jpc;
import org.jpc.converter.JpcConversionException;
import org.jpc.converter.JpcConverter;
import org.jpc.term.Atom;
import org.jpc.term.FloatTerm;
import org.jpc.term.IntegerTerm;
import org.jpc.term.Term;

public class CalendarConverter extends JpcConverter<Calendar, Term> {

	@Override
	public <T extends Term> T toTerm(Calendar calendar, Class<T> termClass, Jpc context) {
		long timeInMilliSeconds = calendar.getTimeInMillis();
		Term term = null;
		if(termClass.isAssignableFrom(IntegerTerm.class))
			term = new IntegerTerm(timeInMilliSeconds);
		else if(termClass.equals(FloatTerm.class))
			term = new FloatTerm(timeInMilliSeconds);
		else if(termClass.equals(Atom.class))
			term = new Atom(String.valueOf(timeInMilliSeconds));
		if(term == null)
			throw new JpcConversionException();
		return (T) term;
	}


	@Override
	public Calendar fromTerm(Term term, Type type, Jpc context) {
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
