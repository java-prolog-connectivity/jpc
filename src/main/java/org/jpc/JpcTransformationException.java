package org.jpc;

/**
 * This exception should be thrown when a problem occurs when transforming a Jpc term representation to another representation or vice-versa
 * @author sergioc
 *
 */
public class JpcTransformationException extends JpcException {

	public JpcTransformationException(String from, String to) {
		super(formatMessage(from, to));
	}
	
	public JpcTransformationException(String from, String to, String reason) {
		super(formatMessage(from, to, reason));
	}
	
	public JpcTransformationException(String from, String to, Exception ex) {
		super(formatMessage(from, to), ex);
	}
	
	private static String formatMessage(String from, String to) {
		return formatMessage(from, to, null);
	}
	
	private static String formatMessage(String from, String to, String reason) {
		StringBuilder sb = new StringBuilder("Impossible to transform the term model from " + from + " to " + to + ". ");
		if(reason != null)
			sb.append("Cause: " + reason);
		return sb.toString();
	}

}
