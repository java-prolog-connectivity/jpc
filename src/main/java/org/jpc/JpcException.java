package org.jpc;

/**
 * The exception for any Jpc specific problems
 * @author sergioc
 *
 */
public class JpcException extends RuntimeException {

	public JpcException() {}
	
	public JpcException(String message) {super(message);}
	
	public JpcException(String message, Exception cause) {super(message, cause);}
	
	public JpcException(Exception cause) {super(cause);}

}
