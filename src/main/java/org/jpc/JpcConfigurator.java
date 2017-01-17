package org.jpc;

/**
 * A configurator for a JpcBuilder.
 * @author sergioc
 *
 */
public interface JpcConfigurator {

	/**
	 * Configures a JpcBuilder.
	 * @param jpcBuilder the JpcBuilder to configure.
	 */
	void configure(JpcBuilder jpcBuilder);
	
}
