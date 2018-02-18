package org.jpc.mapping.converter.catalog.datetime;

import java.sql.Timestamp;

import org.jpc.Jpc;
import org.jpc.mapping.converter.SimpleFromTermConverter;
import org.jpc.mapping.converter.SimpleToTermConverter;
import org.jpc.term.Term;

public class TimestampToNumberTermConverter implements SimpleToTermConverter<Timestamp>, SimpleFromTermConverter<Timestamp> {

    @Override
    public Timestamp fromTerm(final Term term, final Jpc context) {
        long millis = context.fromTerm(term, Long.class);
        return new Timestamp(millis);
    }

    @Override
    public Term toTerm(final Timestamp timestamp, final Jpc context) {
        return context.toTerm(timestamp.getTime());
    }
}
