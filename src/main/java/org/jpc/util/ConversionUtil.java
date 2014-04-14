package org.jpc.util;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.jpc.Jpc;
import org.jpc.term.Term;

public class ConversionUtil {

	public static <T> Map<String,T> toObjectMap(Map<String, Term> termMap, Jpc jpc) {
		Map<String,T> objectMap = new HashMap<>();
		for(Entry<String,Term> entry : termMap.entrySet()) {
			T converted = jpc.fromTerm(entry.getValue());
			objectMap.put(entry.getKey(), converted);
		}
		return objectMap;
	}
	
	public static <T> List<Map<String,T>> toObjectMapList(List<? extends Map<String, Term>> termMapList, Jpc jpc) {
		List<Map<String,T>> list = new ArrayList<>();
		for(Map<String, Term> map : termMapList) {
			list.add(toObjectMap(map, jpc));
		}
		return list;
	}
	
}
