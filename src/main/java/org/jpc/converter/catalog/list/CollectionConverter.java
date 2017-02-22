package org.jpc.converter.catalog.list;

import static org.jconverter.converter.ConversionGoal.conversionGoal;

import java.lang.reflect.Type;
import java.util.Collection;
import java.util.List;

import org.jconverter.converter.DelegateConversionException;
import org.jconverter.converter.TypeDomain;
import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.term.Term;
import org.typetools.typewrapper.TypeWrapper;

public class CollectionConverter<U extends Term,T extends Collection<?>> implements FromTermConverter<U,T> {
	
	@Override
	public T fromTerm(Term listTerm, TypeDomain target, Jpc context) {
		if (!listTerm.isList()) {
			throw new DelegateConversionException(conversionGoal(listTerm, target));
		}
		T collection = null;
		List<Term> listMembers = null;
		try {
			collection = context.instantiate(target.getType()); //instantiate the collection type
			listMembers = listTerm.asList();
		} catch(Exception e) {
			throw new DelegateConversionException(conversionGoal(listTerm, target));
		}
		
		TypeWrapper listTypeWrapper = TypeWrapper.wrap(target.getType());
		Type componentType;
		if(listTypeWrapper.hasActualTypeArguments()) {
			componentType = listTypeWrapper.getActualTypeArguments()[0];
		} else {
			componentType = Object.class;
		}
		
		for(Term term : listMembers) {
			Object convertedMember = context.fromTerm(term, componentType);
			((Collection) collection).add(convertedMember);
		}
		return collection;
	}

}
