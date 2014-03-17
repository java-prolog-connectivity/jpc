package org.jpc;

import java.lang.reflect.Type;
import java.util.List;

import org.jconverter.JConverter;
import org.jconverter.factory.FactoryManager;
import org.jpc.converter.JpcConverterManager;
import org.jpc.term.Compound;
import org.jpc.term.ListTerm;
import org.jpc.term.Term;
import org.jpc.term.jterm.JTermManager;

/**
 * A class providing an interface for the main JPC functionality (such as converting between terms and Java objects)
 * @author sergioc
 *
 */
public abstract class Jpc extends JConverter {
	
	public Jpc(JpcConverterManager converterManager, FactoryManager factoryManager) {
		super(converterManager, factoryManager);
	}
	
	public abstract <T> T fromTerm(Term term);
	
	public abstract <T> T fromTerm(Term term, Type type);
	
	public abstract <T extends Term> T toTerm(Object object);
	
	public abstract <T extends Term> T toTerm(Object object, Class<T> termClass);
	
	public abstract Compound toCompound(Object name, List<?> args);
	
	public abstract ListTerm listTerm(Object ...objects);
	
	public abstract ListTerm listTerm(List<?> objects);

	/**
	 * 
	 * @param object the object which conversion target type to infer.
	 * @return the inferred type.
	 */
	public abstract Type getType(Object object);

	public abstract boolean handleError(Term errorTerm, Term goal);

	public abstract JTermManager getJTermManager();

	public abstract Compound newSoftJTerm(Object ref, Compound compound);
	
	public abstract Compound newSoftJTerm(Object ref);
	
	public abstract Compound newWeakJTerm(Object ref, Compound compound);
	
	public abstract Compound newWeakJTerm(Object ref);
	
	public abstract Compound newJTerm(Object ref);
	
	public abstract Compound newJTerm(Object ref, Compound compound);
	
	public abstract void forgetJTerm(Compound term);
	
	public abstract void forgetJTermRef(Object ref);
	
	public abstract Compound jTerm(Object o);
	
	public abstract <T> T resolveJTerm(Compound compound);
	
}
