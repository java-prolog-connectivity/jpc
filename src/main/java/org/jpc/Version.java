package org.jpc;

/**
 * @author sergioc
 *
 */
public class Version {

	Version(){}
	
	public final int major = 0;
	public final int minor = 0;
	public final int patch = 1;
	public final String status = "alpha";
	
	@Override
	public String toString() {
		return major + "." + minor + "." + patch + "-" + status;
	}
	
}
