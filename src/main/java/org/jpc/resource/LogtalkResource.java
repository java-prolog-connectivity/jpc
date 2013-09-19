package org.jpc.resource;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.minitoolbox.regex.RegExUtil;


public class LogtalkResource extends LogicResource {

	private static final List<String> defaultLogtalkExtensions = Arrays.asList(new String[] {"lgt"});
	
	public static List<String> getLogtalkExtensions() {
		return defaultLogtalkExtensions;
	}
	
	public static boolean hasLogtalkExtension(String name) {
		return RegExUtil.hasExtension(name, defaultLogtalkExtensions);
	}
	
	public static List<LogtalkResource> asLogtalkResources(List<String> names) {
		List<LogtalkResource> resources = new ArrayList<>();
		for(String name : names)
			resources.add(new LogtalkResource(name));
		return resources;
	}
	
	public LogtalkResource(String name) {
		super(name);
	}

}
