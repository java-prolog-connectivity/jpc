package org.jpc.converter.catalog;

import java.lang.reflect.Type;
import java.util.GregorianCalendar;

import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.XMLGregorianCalendar;

import org.jpc.Jpc;
import org.jpc.converter.JpcConversionException;
import org.jpc.converter.JpcConverter;
import org.jpc.term.Term;

public class XmlGregorianCalendarConverter extends JpcConverter<XMLGregorianCalendar, Term> {

	@Override
	public <T extends Term> T toTerm(XMLGregorianCalendar calendar, Class<T> termClass, Jpc context) {
		return new CalendarConverter().toTerm(calendar.toGregorianCalendar(), termClass, context);
	}

	@Override
	public XMLGregorianCalendar fromTerm(Term term, Type type, Jpc context) {
		if(!XMLGregorianCalendar.class.equals(type))
			throw new JpcConversionException();
		try {
			return DatatypeFactory.newInstance().newXMLGregorianCalendar((GregorianCalendar)context.fromTerm(term, GregorianCalendar.class));
		} catch (DatatypeConfigurationException e) {
			throw new RuntimeException(e);
		}
	}
	
}
