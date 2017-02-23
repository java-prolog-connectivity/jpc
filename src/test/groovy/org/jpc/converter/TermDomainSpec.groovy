package org.jpc.converter

import org.jconverter.converter.TypeDomain
import org.jpc.term.Atom
import org.jpc.term.Compound
import org.jpc.term.Term
import spock.lang.Specification

import static java.util.Arrays.asList
import static org.jconverter.converter.TypeDomain.typeDomain
import static org.jpc.converter.TermDomain.termDomain
import static org.jpc.term.Atom.atom
import static org.jpc.term.Compound.compound
import static org.jpc.term.Var.dontCare

class TermDomainSpec extends Specification {

    def 'A TermDomain is a subtype of another TermDomain if the term of the former is subsummed by the term of the latter'() {
        given:
        TypeDomain termDomain1 = termDomain(compound("name", asList(dontCare())))
        TypeDomain termDomain2 = termDomain(compound("name", asList(atom("hello"))))
        TypeDomain termDomain3 = termDomain(compound("name", asList(atom("hello"))))

        expect:
        termDomain1.isSubsetOf(termDomain2)
        termDomain2.isSubsetOf(termDomain3)
    }

    def 'A TermDomain is not a subtype of another TermDomain if the term of the former is not subsummed by the term of the latter'() {
        given:
        TypeDomain termDomain1 = termDomain(compound("name", asList(atom("hello"))))
        TypeDomain termDomain2 = termDomain(compound("x", asList(atom("hello"))))

        expect:
        !termDomain1.isSubsetOf(termDomain2)
    }

    def 'A TermDomain is a subtype of a TypeDomain if the type of the latter is assignable from the term type'() {
        given:
        TypeDomain termDomain1 = termDomain(compound("name", asList(atom("hello"))))
        TypeDomain typeDomain1 = typeDomain(Compound.class)
        TypeDomain typeDomain2 = typeDomain(Term.class)
        TypeDomain typeDomain3 = typeDomain(Object.class)

        expect:
        termDomain1.isSubsetOf(typeDomain1)
        termDomain1.isSubsetOf(typeDomain2)
        termDomain1.isSubsetOf(typeDomain3)
    }

    def 'A TermDomain is not a subtype of a TypeDomain if the type of the latter is not assignable from the term type'() {
        given:
        TypeDomain termDomain1 = termDomain(compound("name", asList(atom("hello"))))
        TypeDomain typeDomain1 = typeDomain(Atom.class)
        TypeDomain typeDomain2 = typeDomain(String.class)

        expect:
        !termDomain1.isSubsetOf(typeDomain1)
        !termDomain1.isSubsetOf(typeDomain2)
    }
}
