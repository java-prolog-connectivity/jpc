package org.jpc.converter.fromterm.fromlistterm;

import java.util.List;

import org.jpc.converter.fromterm.DefaultFromTermConverter;
import org.jpc.converter.fromterm.FromTermConverter;
import org.jpc.term.Term;
import org.minitoolbox.reflection.ReflectionUtil;


public class ListTermToArrayConverter<T> extends ListTermToObjectConverter<T[]> {

	private Class<T> arrayClass;
	
	public ListTermToArrayConverter() {
		this((FromTermConverter<T>) new DefaultFromTermConverter());
	}
	
	public ListTermToArrayConverter(FromTermConverter<T> memberConverter) {
		this(memberConverter, (Class<T>) Object[].class);
	}

	public ListTermToArrayConverter(FromTermConverter<T> memberConverter, Class<T> arrayClass) {
		super(memberConverter);
		this.arrayClass = arrayClass;
	}

	@Override
	public T[] apply(Term term) {
		List<T> collection = new ListTermToListConverter(getMemberConverter()).apply(term);
		T[] array = (T[]) ReflectionUtil.createArray(arrayClass, collection.size());
		for(int i=0; i<collection.size(); i++) {
			array[i] = collection.get(i);
		}
		return array;
	}

}
