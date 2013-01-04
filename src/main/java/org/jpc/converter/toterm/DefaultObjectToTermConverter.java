package org.jpc.converter.toterm;

import java.math.BigDecimal;
import java.util.Enumeration;
import java.util.GregorianCalendar;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;

import javax.xml.datatype.XMLGregorianCalendar;

import org.jpc.JpcException;
import org.jpc.converter.toterm.tolistterm.ArrayToTermConverter;
import org.jpc.converter.toterm.tolistterm.EnumerationToTermConverter;
import org.jpc.converter.toterm.tolistterm.IterableToTermConverter;
import org.jpc.converter.toterm.tolistterm.IteratorToTermConverter;
import org.jpc.converter.toterm.tolistterm.MapToTermConverter;
import org.jpc.term.Atom;
import org.jpc.term.FloatTerm;
import org.jpc.term.IntegerTerm;
import org.jpc.term.Term;
import org.jpc.term.TermConvertable;
import org.jpc.term.Variable;

/**
 * A utility for default conversions to Term types
 * @author sergioc
 *
 */
public class DefaultObjectToTermConverter implements ObjectToTermConverter<Object> {
	
	public Term apply(Object o) {
		Term term = null;
		if(o == null)
			term = Variable.ANONYMOUS_VAR;
		else if(o instanceof TermConvertable)
			term = ((TermConvertable)o).asTerm();
		else if(o instanceof Boolean || o instanceof String || o instanceof StringBuilder || o instanceof StringBuffer || o instanceof Character)
			term = new Atom(o.toString());
		else if(o instanceof Number) {
			if(o instanceof BigDecimal || o instanceof Float || o instanceof Double)
				term = new FloatTerm(((Number)o).doubleValue());
			else
				term = new IntegerTerm(((Number)o).longValue());
		} else if(o instanceof GregorianCalendar) {
			term = new GregorianCalendarToTermConverter().apply((GregorianCalendar)o);
		} else if(o instanceof XMLGregorianCalendar) {
			term = new XmlGregorianCalendarToTermConverter().apply((XMLGregorianCalendar)o);
		} else if (o instanceof Entry) {
			term = new MapEntryToTermConverter(new DefaultObjectToTermConverter(), new DefaultObjectToTermConverter()).apply((Entry)o);
		} else if(o instanceof Map) {
			term = new MapToTermConverter(new MapEntryToTermConverter(new DefaultObjectToTermConverter(), new DefaultObjectToTermConverter())).apply((Map)o);
		} else if(o instanceof Enumeration) {
			term = new EnumerationToTermConverter(new DefaultObjectToTermConverter()).apply((Enumeration)o);
		} else if(o instanceof Object[]) {
			term = new ArrayToTermConverter(new DefaultObjectToTermConverter()).apply((Object[])o);
		} else if(o instanceof Iterable) {
			term = new IterableToTermConverter(new DefaultObjectToTermConverter()).apply((Iterable)o);
		} else if(o instanceof Iterator) {
			term = new IteratorToTermConverter(new DefaultObjectToTermConverter()).apply((Iterator)o);
		}
		else
			throw new JpcException("Impossible to interpret the object " + o + " as a term");
		return term;
	}


}
