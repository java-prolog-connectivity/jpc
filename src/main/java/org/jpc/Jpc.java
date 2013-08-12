package org.jpc;

import java.lang.reflect.Type;
import java.util.List;

import org.jpc.term.Compound;
import org.jpc.term.ListTerm;
import org.jpc.term.Term;

/**
 * A class providing an interface for the main JPC functionality (such as converting between terms and Java objects)
 * This class is inspired by the Gson class from the Gson library (http://code.google.com/p/google-gson/)
 * @author sergioc
 *
 */
public interface Jpc {

	public static final Version version = new Version();
	
	public <T> T fromTerm(Term term);
	
	public <T> T fromTerm(Term term, Type type);
	
	public Term toTerm(Object object);
	
	public <T extends Term> T toTerm(Object object, Class<T> termClass);

	public Compound toTerm(Object name, List<? extends Object> args);
	
	public ListTerm listTerm(Object ...objects);
	
	public ListTerm listTerm(List<? extends Object> objects);
	
	public <T> T instantiate(Type targetType);

	public Type getType(Term term);

	public boolean handleError(Term errorTerm, Term goal);

}
