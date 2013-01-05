package org.jpc.converter.fromterm.fromlistterm;

import java.util.List;

import org.jpc.converter.fromterm.DefaultTermToObjectConverter;
import org.jpc.converter.fromterm.TermToObjectConverter;
import org.jpc.term.Term;
import org.minitoolbox.reflection.ReflectionUtil;


public class ListTermToArrayConverter<T> extends ListTermToObjectConverter<T[]> {

	private Class<T> arrayClass;
	
	public ListTermToArrayConverter() {
		this((TermToObjectConverter<T>) new DefaultTermToObjectConverter());
	}
	
	public ListTermToArrayConverter(TermToObjectConverter<T> memberConverter) {
		this(memberConverter, (Class<T>) Object[].class);
	}

	public ListTermToArrayConverter(TermToObjectConverter<T> memberConverter, Class<T> arrayClass) {
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
