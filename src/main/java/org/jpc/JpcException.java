package org.jpc;

public class JpcException extends RuntimeException {

	public JpcException() {}
	
	public JpcException(String message) {super(message);}
	
	public JpcException(String message, Exception cause) {super(message, cause);}
	
}
