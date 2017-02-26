package org.jpc.mapping.converter.catalog.primitive;

import org.jconverter.converter.TypeDomain;
import org.jpc.Jpc;
import org.jpc.mapping.converter.FromTermConverter;
import org.jpc.mapping.converter.ToTermConverter;
import org.jpc.term.Number;

public class CharacterToNumberTermConverter<T extends Number> implements ToTermConverter<Character, T>, FromTermConverter<T, Character> {

	@Override
	public Character fromTerm(T term, TypeDomain target, Jpc context) {
		String s = context.convert(term, String.class);
		return context.convert(s, Character.class);
	}

	@Override
	public T toTerm(Character character, TypeDomain target, Jpc context) {
		return context.toTerm(character.toString(), target);
	}

}
