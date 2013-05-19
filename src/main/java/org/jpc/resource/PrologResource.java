package org.jpc.resource;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.minitoolbox.RegExUtil;


public class PrologResource extends LogicResource {
	
	private static final List<String> defaultPrologExtensions = Arrays.asList(new String[] {"pl", "P", "pro"});
	
	public static List<String> getPrologExtensions() {
		return defaultPrologExtensions;
	}
	
	public static boolean hasPrologExtension(String name) {
		return RegExUtil.hasExtension(name, defaultPrologExtensions);
	}
	
	public static List<PrologResource> asPrologResources(List<String> names) {
		List<PrologResource> resources = new ArrayList<>();
		for(String name : names)
			resources.add(new PrologResource(name));
		return resources;
	}
	
	public PrologResource(String name) {
		super(name);
	}

}
