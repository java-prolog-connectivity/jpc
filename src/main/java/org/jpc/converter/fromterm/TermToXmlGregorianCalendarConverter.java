package org.jpc.converter.fromterm;

import java.lang.reflect.Type;
import java.util.GregorianCalendar;

import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.XMLGregorianCalendar;

import org.jpc.Jpc;
import org.jpc.converter.JpcConversionException;
import org.jpc.term.Term;

public class TermToXmlGregorianCalendarConverter extends FromTermConverter<XMLGregorianCalendar>{

	@Override
	public XMLGregorianCalendar convert(Term term, Type type, Jpc context) {
		if(!XMLGregorianCalendar.class.equals(type))
			throw new JpcConversionException();
		try {
			return DatatypeFactory.newInstance().newXMLGregorianCalendar((GregorianCalendar)context.fromTerm(term, GregorianCalendar.class));
		} catch (DatatypeConfigurationException e) {
			throw new RuntimeException(e);
		}
	}

}
