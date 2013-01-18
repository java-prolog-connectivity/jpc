package org.jpc;

import java.lang.reflect.Type;

import org.jpc.converter.fromterm.DefaultFromTermConverter;
import org.jpc.converter.fromterm.FromTermConverter;
import org.jpc.converter.toterm.DefaultToTermConverter;
import org.jpc.converter.toterm.ToTermConverter;
import org.jpc.term.Term;
import org.minitoolbox.exception.NotImplementedException;

/**
 * A syntactic sugar class for abstracting common JPC functionality (such as converting between terms and Java objects)
 * This class is inspired by the Gson and GsonBuilder class from the Gson library (http://code.google.com/p/google-gson/)
 * @author sergioc
 *
 */
public class Jpc {

	public Term toTerm(Object object) {
		return new DefaultToTermConverter().apply(object);
	}
	
	public <T> T fromTerm(Term term, Class<T> clazz) {
		return (T)new DefaultFromTermConverter().apply(term);
	}
	
	//TODO
	public void registerTypeConverter(Type type, FromTermConverter converter) {
		throw new NotImplementedException();
	}
	
	//TODO
	public void registerTypeConverter(Type type, ToTermConverter converter) {
		throw new NotImplementedException();
	}
	
}
