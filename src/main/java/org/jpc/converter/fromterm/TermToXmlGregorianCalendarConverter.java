package org.jpc.converter.fromterm;

import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.XMLGregorianCalendar;

import org.jpc.term.Term;

public class TermToXmlGregorianCalendarConverter implements FromTermConverter<XMLGregorianCalendar>{

	@Override
	public XMLGregorianCalendar apply(Term term) {
		try {
			return DatatypeFactory.newInstance().newXMLGregorianCalendar(new TermToGregorianCalendarConverter().apply(term));
		} catch (DatatypeConfigurationException e) {
			throw new RuntimeException(e);
		}
	}

}
