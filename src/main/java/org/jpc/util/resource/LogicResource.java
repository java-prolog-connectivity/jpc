package org.jpc.util.resource;

import static org.jpc.util.dialect.Language.LOGTALK;
import static org.jpc.util.dialect.Language.PROLOG;

import java.util.ArrayList;
import java.util.List;

import com.google.common.base.Joiner;

public abstract class LogicResource {

	public static boolean hasLogicExtension(String name) {
		return PrologResource.hasPrologExtension(name) || LogtalkResource.hasLogtalkExtension(name);
	}
	
	private static String logicResourceExtensionRegex; //an OR regex expression with all the extensions of logic resources
	
	public static List<String> getLogicResourceExtensions() {
		List<String> logicResourceExtensions = new ArrayList<String>(PROLOG.getExtensions());
		logicResourceExtensions.addAll(LOGTALK.getExtensions());
		return logicResourceExtensions;
	}
	
	private static String findLogicResourceExtensionRegex() {
		return Joiner.on("|").join(getLogicResourceExtensions());
	}
	
	public static String getLogicResourceExtensionRegex() {
		if(logicResourceExtensionRegex == null)
			logicResourceExtensionRegex = findLogicResourceExtensionRegex();
		return logicResourceExtensionRegex;
	}

	
	public static String suppressLogicExtension(String name) {
		return name.trim().replace("\\.("+getLogicResourceExtensionRegex()+")$", "");
	}
	
	public static List<String> suppressLogicExtensions(List<String> names) {
		List<String> fileNames = new ArrayList<>();
		for(String s : names) {
			fileNames.add(suppressLogicExtension(s));
		}
		return fileNames;
	}

	protected final String name;

	public LogicResource(String name) {
		this.name = name.trim();
	}

	public String getName() {
		return name;
	}

	public String getNameWithoutLogicExtension() {
		return suppressLogicExtension(name);
	}

	@Override
	public String toString() {return name;}

}
