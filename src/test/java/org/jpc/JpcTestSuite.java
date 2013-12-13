package org.jpc;

import org.jpc.converter.DefaultTermConverterTest;
import org.jpc.converter.TypeSolverTest;
import org.jpc.salt.JpcTermWriterTest;
import org.jpc.term.AbstractTermTest;
import org.jpc.term.AtomTest;
import org.jpc.term.CompoundTest;
import org.jpc.term.FloatTermTest;
import org.jpc.term.HilogTermTest;
import org.jpc.term.IntegerTermTest;
import org.jpc.term.ListTermTest;
import org.jpc.term.VariableTest;
import org.jpc.term.expansion.ParameterizedSymbolExpanderTest;
import org.jpc.util.PrologUtilTest;
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
	HilogTermTest.class,
	TypeSolverTest.class,
	DefaultTermConverterTest.class,
	PrologUtilTest.class,
	JpcTermWriterTest.class,
	ParameterizedSymbolExpanderTest.class
	})
public class JpcTestSuite {}
