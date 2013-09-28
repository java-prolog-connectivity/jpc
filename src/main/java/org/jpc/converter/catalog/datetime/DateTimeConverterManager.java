package org.jpc.converter.catalog.datetime;

import org.jpc.converter.ConverterManager;
import org.jpc.term.Term;

public class DateTimeConverterManager extends ConverterManager<Object, Term> {

	public DateTimeConverterManager() {
		register(new XmlGregorianCalendarConverter());
		register(new CalendarConverter());
	}
}
