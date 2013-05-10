package org.jpc.resource;

import java.util.ArrayList;
import java.util.List;

import com.google.common.base.Joiner;

public abstract class LogicResource extends AbstractResource {

	public static boolean hasLogicExtension(String name) {
		return PrologResource.hasPrologExtension(name) || LogtalkResource.hasLogtalkExtension(name);
	}
	
	private static String logicResourceExtensionRegex; //an OR regex expression with all the extensions of logic resources
	
	public static List<String> getLogicResourceExtensions() {
		List<String> logicResourceExtensions = new ArrayList<String>(PrologResource.getPrologExtensions());
		logicResourceExtensions.addAll(LogtalkResource.getLogtalkExtensions());
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

	public String getNameWithoutLogicExtension() {
		return suppressLogicExtension(name);
	}
	
	public LogicResource(String name) {
		super(name);
	}

}
