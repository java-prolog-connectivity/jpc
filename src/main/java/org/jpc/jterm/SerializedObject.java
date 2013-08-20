package org.jpc.jterm;

import static java.util.Arrays.asList;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectOutput;
import java.io.ObjectOutputStream;
import java.io.Serializable;

import javax.xml.bind.DatatypeConverter;

import org.jpc.converter.TermConvertable;
import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.Term;

public class SerializedObject implements TermConvertable<Compound> {

	public static final String SERIALIZED_TERM_FUNCTOR = "jserialized";
	
	public static Term serializedObjectTerm(Serializable serializable) {
		return new SerializedObject(serializable).asTerm();
	}
	
	private Serializable serializable;
	private String encodedBytes;
	
	public SerializedObject(Serializable serializable) {
		this.serializable = serializable;
		byte[] bytes;
		try(ByteArrayOutputStream bos = new ByteArrayOutputStream(); 
				ObjectOutput out = new ObjectOutputStream(bos)) {
			out.writeObject(serializable);
			bytes = bos.toByteArray();
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
		//see: http://stackoverflow.com/questions/20778/how-do-you-convert-binary-data-to-strings-and-back-in-java
		encodedBytes = DatatypeConverter.printBase64Binary(bytes);
	}

	public Serializable getSerializable() {
		return serializable;
	}

	public String getEncodedBytes() {
		return encodedBytes;
	}

	@Override
	public Compound asTerm() {
		return new Compound(SERIALIZED_TERM_FUNCTOR, asList(new Atom(encodedBytes)));
	}
	
}
