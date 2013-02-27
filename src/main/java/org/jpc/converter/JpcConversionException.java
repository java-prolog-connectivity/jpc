package org.jpc.converter;

import org.jpc.JpcException;

/**
 * This exception should be thrown when a problem occurs when transforming an arbitrary object to its Jpc term representation or vice-versa
 * @author sergioc
 *
 */
public class JpcConversionException extends JpcException {

	public JpcConversionException() {
	}
	
	public JpcConversionException(String from, String to) {
		super(formatMessage(from, to));
	}
	
	public JpcConversionException(String from, String to, String reason) {
		super(formatMessage(from, to, reason));
	}
	
	public JpcConversionException(String from, String to, Exception ex) {
		super(formatMessage(from, to), ex);
	}
	
	
	
	private static String formatMessage(String from, String to) {
		return formatMessage(from, to, null);
	}
	
	private static String formatMessage(String from, String to, String reason) {
		StringBuilder sb = new StringBuilder("Impossible to convert from " + from + " to " + to + ". ");
		if(reason != null)
			sb.append("Cause: " + reason);
		return sb.toString();
	}

}
