package org.jpc.util.resource;

import static org.jpc.engine.dialect.Language.LOGTALK;

import java.util.ArrayList;
import java.util.List;

import org.jpc.internal.regex.RegExUtil;


public class LogtalkResource extends LogicResource {

	public static boolean hasLogtalkExtension(String name) {
		return RegExUtil.hasExtension(name, LOGTALK.getExtensions());
	}

	public static List<LogtalkResource> asLogtalkResources(List<String> names) {
		List<LogtalkResource> resources = new ArrayList<>();
		for (String name : names)
			resources.add(new LogtalkResource(name));
		return resources;
	}

	public LogtalkResource(String name) {
		super(name);
	}

}
