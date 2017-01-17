package org.jpc.converter.catalog.primitive;

import java.lang.reflect.Type;

import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.converter.ToTermConverter;
import org.jpc.term.Number;

public class CharacterToNumberTermConverter<T extends Number> implements ToTermConverter<Character, T>, FromTermConverter<T, Character> {

	@Override
	public Character fromTerm(T term, Type targetType, Jpc context) {
		String s = context.convert(term, String.class);
		return context.convert(s, Character.class);
	}

	@Override
	public T toTerm(Character character, Class<T> termClass, Jpc context) {
		return context.toTerm(character.toString(), termClass);
	}

}
