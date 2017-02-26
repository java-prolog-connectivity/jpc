package org.jpc.term;

import static java.util.Arrays.asList;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInput;
import java.io.ObjectInputStream;
import java.io.ObjectOutput;
import java.io.ObjectOutputStream;
import java.io.Serializable;

import javax.xml.bind.DatatypeConverter;

import org.jpc.mapping.converter.TermConvertable;

public class SerializedTerm implements TermConvertable<Compound> {

	public static final String SERIALIZED_TERM_FUNCTOR = "jserialized";
	
	public static Compound serialize(Serializable serializable) {
		return new SerializedTerm(serializable).asTerm();
	}
	
	public static <T extends Serializable> T deserialize(byte[] bytes) {
		try(ByteArrayInputStream bis = new ByteArrayInputStream(bytes);
				ObjectInput in = new ObjectInputStream(bis)) {
			return (T) in.readObject();
		} catch (IOException | ClassNotFoundException e) {
			throw new RuntimeException(e);
		}
	}
	
	private Serializable serializable;
	private String encodedBytes;
	
	public SerializedTerm(Serializable serializable) {
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
