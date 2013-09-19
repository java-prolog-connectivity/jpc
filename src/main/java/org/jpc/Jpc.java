package org.jpc;

import java.lang.reflect.Type;
import java.util.Arrays;
import java.util.List;

import org.jpc.term.Compound;
import org.jpc.term.ListTerm;
import org.jpc.term.Term;
import org.minitoolbox.commons.Version;

/**
 * A class providing an interface for the main JPC functionality (such as converting between terms and Java objects)
 * This class is inspired by the Gson class from Google's Gson library (http://code.google.com/p/google-gson/)
 * @author sergioc
 *
 */
public abstract class Jpc {

	public static final Version version = new Version(0,0,1,"alpha");
	
	public final <T> T fromTerm(Term term) {
		return fromTerm(term, Object.class);
	}
	
	public abstract <T> T fromTerm(Term term, Type type);
	
	public final Term toTerm(Object object) {
		return toTerm(object, Term.class);
	}
	
	public final Compound toTerm(Object name, List<? extends Object> args) {
		return new Compound(toTerm(name), listTerm(args));
	}
	
	public abstract <T extends Term> T toTerm(Object object, Class<T> termClass);
	
	public final ListTerm listTerm(Object ...objects) {
		return listTerm(Arrays.asList(objects));
	}
	
	public final ListTerm listTerm(List<? extends Object> objects) {
		ListTerm listTerm = new ListTerm();
		for(Object o : objects) {
			listTerm.add(toTerm(o));
		}
		return listTerm;
	}
	
	public abstract <T> T instantiate(Type targetType);

	public abstract Type getType(Term term);

	public abstract boolean handleError(Term errorTerm, Term goal);

}
