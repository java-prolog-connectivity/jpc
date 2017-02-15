package org.jpc.converter.catalog.reflection;


import static org.junit.Assert.assertEquals;

import org.jpc.Jpc;
import org.jpc.JpcBuilder;
import org.jpc.term.Term;
import org.junit.Test;

public class EnumConverterTest {

    Jpc jpc = JpcBuilder.create().build();

    public static enum E {X}

    @Test
    public void testEnumConversion() {
        E myEnum = E.X;
        Term enumTerm = jpc.toTerm(E.X);
        assertEquals(E.X, jpc.fromTerm(enumTerm));
    }

}
