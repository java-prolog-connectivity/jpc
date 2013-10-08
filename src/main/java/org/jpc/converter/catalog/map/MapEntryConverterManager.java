package org.jpc.converter.catalog.map;

import java.util.Map;

import org.jpc.converter.ConverterManager;
import org.jpc.converter.catalog.map.MapEntryConverter.MapEntryToTermConverter;
import org.jpc.converter.catalog.map.MapEntryConverter.TermToMapEntryConverter;
import org.jpc.converter.typesolver.MapTypeSolver;
import org.jpc.term.Compound;

public class MapEntryConverterManager extends ConverterManager<Map.Entry, Compound> {

	public MapEntryConverterManager() {
		register(new MapEntryToTermConverter(MapTypeSolver.DEFAULT_MAP_ENTRY_SEPARATOR));
		for(String mapEntrySeparator : MapTypeSolver.ALL_MAP_ENTRY_SEPARATORS) {
			register(new TermToMapEntryConverter(mapEntrySeparator));
		}
	}
	
}
