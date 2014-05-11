package org.jpc.term.compiler;

public class Environment {

	private static int ID_COUNTER = -1;
	
	private final int id;
	
	public Environment() {
		this(ID_COUNTER--); //in the current implementation default environment ids are negative numbers, positive ids are reserved for clauses environments.
	}
	
	public Environment(int id) {
		this.id = id;
	}
	
	public int getId() {
		return id;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + id;
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		Environment other = (Environment) obj;
		if (id != other.id)
			return false;
		return true;
	}
	
}
