package org.jpc.mapping.converter

import org.jconverter.converter.TypeDomain
import org.jpc.Jpc
import org.jpc.term.Compound

import static java.util.Arrays.asList
import static org.jpc.term.Atom.atom
import static org.jpc.term.Compound.compound

class ConvertersFixture {

    static class HelloConverter implements ToTermConverter<String, Compound> {

        Compound toTerm(String string, TypeDomain target, Jpc context) {
            return compound("hello", asList(atom(string)))
        }
    }

    static class ByeConverter implements ToTermConverter<String, Compound> {

        Compound toTerm(String string, TypeDomain target, Jpc context) {
            return compound("bye", asList(atom(string)))
        }
    }

}
