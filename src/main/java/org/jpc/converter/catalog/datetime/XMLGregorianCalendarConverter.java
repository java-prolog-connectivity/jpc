package org.jpc.converter.catalog.datetime;

import java.util.GregorianCalendar;

import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.XMLGregorianCalendar;

import org.jconverter.converter.TypeDomain;
import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.converter.ToTermConverter;
import org.jpc.term.Term;

public class XMLGregorianCalendarConverter<T extends Term> implements ToTermConverter<XMLGregorianCalendar, T>, FromTermConverter<T, XMLGregorianCalendar> {

	@Override
	public T toTerm(XMLGregorianCalendar calendar, TypeDomain target, Jpc context) {
		return context.toTerm(calendar.toGregorianCalendar(), target);
	}

	@Override
	public XMLGregorianCalendar fromTerm(T term, TypeDomain target, Jpc context) {
		GregorianCalendar gregCalendar = context.fromTerm(term, GregorianCalendar.class);
		try {
			return DatatypeFactory.newInstance().newXMLGregorianCalendar(gregCalendar);
		} catch (DatatypeConfigurationException e) {
			throw new RuntimeException(e);
		}
	}
	
}
