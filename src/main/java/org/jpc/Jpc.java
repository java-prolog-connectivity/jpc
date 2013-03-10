package org.jpc;

import java.lang.reflect.Type;
import java.util.Arrays;
import java.util.List;

import org.jpc.converter.ConverterManager;
import org.jpc.converter.DefaultConverterManager;
import org.jpc.instantiationmanager.DefaultInstantiationManager;
import org.jpc.instantiationmanager.InstantiationManager;
import org.jpc.term.Compound;
import org.jpc.term.ListTerm;
import org.jpc.term.Term;
import org.jpc.typesolver.DefaultTypeSolverManager;
import org.jpc.typesolver.TypeSolverManager;

/**
 * A class providing an interface for the main JPC functionality (such as converting between terms and Java objects)
 * This class is inspired by the Gson and GsonBuilder class from the Gson library (http://code.google.com/p/google-gson/)
 * @author sergioc
 *
 */
public class Jpc {

	private ConverterManager converterManager;
	private TypeSolverManager typeSolverManager;
	private InstantiationManager instantiationManager;
	//private JpcPreferences preferences;
	
	public Jpc() {
		this.converterManager = new DefaultConverterManager();
		this.typeSolverManager = new DefaultTypeSolverManager();
		this.instantiationManager = new DefaultInstantiationManager();
	}
	
	public Jpc(ConverterManager converterManager, TypeSolverManager typeSolverManager, InstantiationManager instantiationManager) {
		this.typeSolverManager = typeSolverManager;
		this.converterManager = converterManager;
		this.instantiationManager = instantiationManager;
		//this.preferences = preferences;
	}
	
	public Compound compound(String name, List<? extends Object> args) {
		return new Compound(name, listTerm(args));
	}
	
	public ListTerm listTerm(Object ...objects) {
		return listTerm(Arrays.asList(objects));
	}
	
	public ListTerm listTerm(List<? extends Object> objects) {
		ListTerm listTerm = new ListTerm();
		for(Object o : objects) {
			listTerm.add(toTerm(o));
		}
		return listTerm;
	}
	
	public Object fromTerm(Term term) {
		return fromTerm(term, Object.class);
	}
	
	public <T> T fromTerm(Term term, Type type) {
		return (T) converterManager.fromTerm(term, type, this);
	}
	
	public Term toTerm(Object object) {
		return toTerm(object, Term.class);
	}
	
	public <T extends Term> T toTerm(Object object, Class<T> termClass) {
		return converterManager.toTerm(object, termClass, this);
	}
	

	
	public <T> T instantiate(Type t) {
		return instantiationManager.instantiate(t);
	}

	public Type getType(Term term) {
		return typeSolverManager.getType(term);
	}

}
