package org.jpc.converter.toterm.tolistterm;

import java.util.Map;

import org.jpc.converter.toterm.MapEntryToTermConverter;
import org.jpc.converter.toterm.ToTermConverter;
import org.jpc.term.Term;

public class MapToTermConverter extends ObjectToListTermConverter<Map>{

	public MapToTermConverter() {
		this(new MapEntryToTermConverter());
	}
	
	public MapToTermConverter(ToTermConverter memberConverter) {
		super(memberConverter);
	}
	
	@Override
	public Term apply(Map map) {
		return new IterableToTermConverter(getMemberConverter()).apply(map.entrySet());
	}

}
