package org.jpc.converter.toterm;

import javax.xml.datatype.XMLGregorianCalendar;

import org.jpc.Jpc;
import org.jpc.term.Term;

public class XmlGregorianCalendarToTermConverter extends ToTermConverter<XMLGregorianCalendar> {

	@Override
	public Term convert(XMLGregorianCalendar calendar, Jpc context) {
		return new CalendarToTermConverter().convert(calendar.toGregorianCalendar(), context);
	}

}
