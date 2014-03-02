package org.jpc;

import org.jpc.converter.CustomTermConverterTest;
import org.jpc.converter.DefaultConverterTest;
import org.jpc.converter.TypeSolverTest;
import org.jpc.engine.embedded.JpcEmbeddedEngineTestSuite;
import org.jpc.salt.JpcTermWriterTest;
import org.jpc.term.AbstractTermTest;
import org.jpc.term.AtomTest;
import org.jpc.term.CompoundTest;
import org.jpc.term.FloatTermTest;
import org.jpc.term.HilogTermTest;
import org.jpc.term.IntegerTermTest;
import org.jpc.term.JRefTest;
import org.jpc.term.JTermTest;
import org.jpc.term.ListTermTest;
import org.jpc.term.SerializedTermTest;
import org.jpc.term.VariableTest;
import org.jpc.term.expansion.ParameterizedSymbolExpanderTest;
import org.jpc.term.unification.UnificationTest;
import org.jpc.util.PrologUtilTest;
import org.jpc.util.config.EngineConfigurationDeserializerTest;
import org.jpc.util.config.JpcConfigurationDeserializerTest;
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
	JpcTermWriterTest.class,
	ParameterizedSymbolExpanderTest.class,
	UnificationTest.class,
	JpcEmbeddedEngineTestSuite.class,
	JRefTest.class,
	JTermTest.class,
	SerializedTermTest.class,
	TypeSolverTest.class,
	DefaultConverterTest.class,
	CustomTermConverterTest.class,
	PrologUtilTest.class,
	EngineConfigurationDeserializerTest.class,
	JpcConfigurationDeserializerTest.class
})
public class JpcTestSuite {}
