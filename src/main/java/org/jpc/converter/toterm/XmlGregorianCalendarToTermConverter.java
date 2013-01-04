package org.jpc.converter.toterm;

import javax.xml.datatype.XMLGregorianCalendar;

import org.jpc.term.Term;

public class XmlGregorianCalendarToTermConverter implements ObjectToTermConverter<XMLGregorianCalendar> {

	@Override
	public Term apply(XMLGregorianCalendar calendar) {
		return new GregorianCalendarToTermConverter().apply(calendar.toGregorianCalendar());
	}

}
