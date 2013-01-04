package org.jpc.converter.fromterm.fromlistterm;

import java.util.ArrayList;
import java.util.List;

import org.jpc.converter.fromterm.TermToObjectConverter;
import org.jpc.term.Term;
import org.minitoolbox.reflection.ReflectionUtil;


public class ListTermToArrayConverter<T> extends ListTermToObjectConverter<T[]> {

	private Class<T> arrayClass;
	
	public ListTermToArrayConverter(TermToObjectConverter memberConverter) {
		this(memberConverter, (Class<T>) Object[].class);
	}

	public ListTermToArrayConverter(TermToObjectConverter memberConverter, Class<T> arrayClass) {
		super(memberConverter);
		this.arrayClass = arrayClass;
	}

	@Override
	public T[] apply(Term term) {
		List<T> collection = (List<T>)new ListTermToCollectionConverter(getMemberConverter(), ArrayList.class).apply(term);
		T[] array = (T[]) ReflectionUtil.createArray(arrayClass, collection.size());
		for(int i=0; i<collection.size(); i++) {
			array[i] = collection.get(i);
		}
		return array;
	}

}
