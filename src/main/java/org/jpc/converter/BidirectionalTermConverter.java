package org.jpc.converter;

import org.jpc.term.Term;

/**
 * An auxiliary interface for facilitating the declaration of classes having both the responsibility of converting a term to an object and vice versa.
 * @author sergioc
 *
 * @param <T> the object type.
 * @param <V> the term type.
 */
public interface BidirectionalTermConverter<T,V extends Term> extends ToTermConverter<T,V>, FromTermConverter<V,T> {

}
