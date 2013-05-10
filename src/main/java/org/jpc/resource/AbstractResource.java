package org.jpc.resource;


public abstract class AbstractResource {

	protected final String name;

	public AbstractResource(String name) {
		this.name = name.trim();
	}
	
	public String getName() {
		return name;
	}

	@Override
	public String toString() {return name;}
	
}
