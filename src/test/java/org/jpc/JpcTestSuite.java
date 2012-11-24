package org.jpc;

import org.jpc.term.AbstractTermTest;
import org.jpc.term.AtomTest;
import org.jpc.term.CompoundTest;
import org.jpc.term.FloatTermTest;
import org.jpc.term.IntegerTermTest;
import org.jpc.term.ListTermTest;
import org.jpc.term.VariableTest;
import org.jpc.util.DefaultTermConverterTest;
import org.jpc.visitor.JpcWriterVisitorTest;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;

@RunWith(Suite.class)
@Suite.SuiteClasses({
	VariableTest.class,
	IntegerTermTest.class,
	FloatTermTest.class,
	AtomTest.class,
	CompoundTest.class,
	ListTermTest.class,
	AbstractTermTest.class,
	DefaultTermConverterTest.class,
	JpcWriterVisitorTest.class
	})
public class JpcTestSuite {}
