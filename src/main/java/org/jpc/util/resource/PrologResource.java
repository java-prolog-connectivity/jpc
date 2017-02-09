package org.jpc.util.resource;

import static org.jpc.engine.dialect.Language.PROLOG;

import java.util.ArrayList;
import java.util.List;

import org.jpc.internal.regex.RegExUtil;


public class PrologResource extends LogicResource {

	public static boolean hasPrologExtension(String name) {
		return RegExUtil.hasExtension(name, PROLOG.getExtensions());
	}
	
	public static List<PrologResource> asPrologResources(List<String> names) {
		List<PrologResource> resources = new ArrayList<>();
		for (String name : names) {
			resources.add(new PrologResource(name));
		}
		return resources;
	}
	
	public PrologResource(String name) {
		super(name);
	}

}
