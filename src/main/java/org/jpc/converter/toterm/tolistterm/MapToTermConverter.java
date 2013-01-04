package org.jpc.converter.toterm.tolistterm;

import java.util.Map;

import org.jpc.converter.toterm.ObjectToTermConverter;
import org.jpc.term.Term;

public class MapToTermConverter extends ObjectToListTermConverter<Map>{

	public MapToTermConverter(ObjectToTermConverter memberConverter) {
		super(memberConverter);
	}
	
	@Override
	public Term apply(Map map) {
		return new IterableToTermConverter(getMemberConverter()).apply(map.entrySet());
	}

}
