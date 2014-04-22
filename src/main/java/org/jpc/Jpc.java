package org.jpc;

import java.lang.reflect.Type;
import java.util.List;

import org.jconverter.JConverter;
import org.jconverter.factory.FactoryManager;
import org.jpc.converter.JpcConverterManager;
import org.jpc.term.Compound;
import org.jpc.term.ListTerm;
import org.jpc.term.Term;
import org.jpc.term.refterm.RefTermManager;

/**
 * A class providing an interface for the main JPC functionality (such as converting between terms and Java objects)
 * @author sergioc
 *
 */
public abstract class Jpc extends JConverter {
	
	private static Jpc defaultJpc;
	
	public static Jpc getDefault() {
		if(defaultJpc == null)
			setDefault(new DefaultJpc());
		return defaultJpc;
	}
	
	public static void setDefault(Jpc jpc) {
		defaultJpc = jpc;
	}
	
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

	public abstract RefTermManager getRefTermManager();

	public abstract Compound newSoftRefTerm(Object ref, Compound compound);
	
	public abstract Compound newSoftRefTerm(Object ref);
	
	public abstract Compound newWeakRefTerm(Object ref, Compound compound);
	
	public abstract Compound newWeakRefTerm(Object ref);
	
	public abstract Compound newRefTerm(Object ref);
	
	public abstract Compound newRefTerm(Object ref, Compound compound);
	
	public abstract void forgetRefTerm(Compound term);
	
	public abstract void forgetRef(Object ref);
	
	public abstract Compound refTerm(Object o);
	
	public abstract <T> T resolveRefTerm(Compound compound);
	
}
