package org.jpc.converter.catalog.datetime;

import java.lang.reflect.Type;
import java.util.GregorianCalendar;

import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.XMLGregorianCalendar;

import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.converter.ToTermConverter;
import org.jpc.term.Term;

public class XmlGregorianCalendarConverter<T extends Term> implements ToTermConverter<XMLGregorianCalendar, T>, FromTermConverter<T, XMLGregorianCalendar> {

	@Override
	public T toTerm(XMLGregorianCalendar calendar, Class<T> termClass, Jpc context) {
		return context.toTerm(calendar.toGregorianCalendar(), termClass);
	}

	@Override
	public XMLGregorianCalendar fromTerm(T term, Type targetType, Jpc context) {
		GregorianCalendar gregCalendar = context.fromTerm(term, GregorianCalendar.class);
		try {
			return DatatypeFactory.newInstance().newXMLGregorianCalendar(gregCalendar);
		} catch (DatatypeConfigurationException e) {
			throw new RuntimeException(e);
		}
	}
	
}
