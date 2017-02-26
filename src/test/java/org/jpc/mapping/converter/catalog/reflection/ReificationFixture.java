package org.jpc.mapping.converter.catalog.reflection;

import java.lang.reflect.Type;
import java.util.List;

import org.jpc.JpcBuilder;
import org.jpc.term.Atom;

import com.google.common.reflect.TypeToken;

public class ReificationFixture {

	private static final String packageName = B.class.getPackage().getName();
	private static final Type atomListType = new TypeToken<List<Atom>>(){}.getType();
	public List<Atom> packageFragmentAtoms = JpcBuilder.create().build().convert(packageName.split("[.]"), atomListType);
	
	
	public static class A{
		public static long m = 10;
		public long n = 10;
		
		public static long m(long n) {return n;}
		
		public long n() {return n;}
	}
	
	public static class B extends A {
	}
	
}
