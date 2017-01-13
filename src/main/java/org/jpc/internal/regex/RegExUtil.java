package org.jpc.internal.regex;

import static java.util.Arrays.asList;

import java.util.List;
import java.util.regex.Pattern;

import com.google.common.base.Joiner;

public class RegExUtil {

	public static boolean hasExtension(String fileName, String ...extensions) {
		return hasExtension(fileName, asList(extensions));
	}
	
	public static boolean hasExtension(String fileName, List<String> extensions) {
		String regex = ".*\\.("+Joiner.on("|").join(extensions)+")$";
		Pattern fileExtensionPattern = Pattern.compile(regex);
		return fileExtensionPattern.matcher(fileName.trim()).matches();
	}
	
}
