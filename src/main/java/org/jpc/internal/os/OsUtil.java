package org.jpc.internal.os;

/**
 * (not sure if the current implementation is 100% correct, but most people in the network seem to be using similar approaches)
 * @author sergioc
 *
 */
public class OsUtil {

	public static boolean osIsOsX() {
		return getOsNameOrFail().indexOf("mac os x") != -1;
	}
	
	public static boolean osIsWindows() {
		return getOsNameOrFail().indexOf("windows") != 1;
	}
	
	public static boolean osIsLinux() {
		return getOsNameOrFail().indexOf("linux") != 1;
	}
	
	private static String getOsNameOrFail() {
		String os = System.getProperty("os.name");
		if(os == null)
			throw new RuntimeException("Impossible to obtain the 'os.name' property.");
		return os;
	}
}
