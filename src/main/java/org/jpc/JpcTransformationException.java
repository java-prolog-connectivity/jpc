package org.jpc;

public class JpcTransformationException extends JpcException {

	private String from;
	private String to;
	private String reason;
	
	
	public JpcTransformationException(String from, String to) {
		this(from, to, null);
	}
	
	public JpcTransformationException(String from, String to, String reason) {
		this.from = from;
		this.to = to;
		this.reason = reason;
	}
	
	@Override
	public String getMessage() {
		StringBuilder sb = new StringBuilder("Impossible to transform the term model from " + from + " to " + to + ". ");
		if(reason != null)
			sb.append("Cause: " + reason);
		return sb.toString();
	}

}
