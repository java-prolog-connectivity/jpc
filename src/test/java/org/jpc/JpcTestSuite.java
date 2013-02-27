package org.jpc;

import org.jpc.converter.DefaultTermConverterTest;
import org.jpc.instantiationmanager.InstantiationManagerTest;
import org.jpc.salt.JpcTermWriterTest;
import org.jpc.term.AbstractTermTest;
import org.jpc.term.AtomTest;
import org.jpc.term.CompoundTest;
import org.jpc.term.FloatTermTest;
import org.jpc.term.IntegerTermTest;
import org.jpc.term.ListTermTest;
import org.jpc.term.VariableTest;
import org.jpc.util.LogicUtilTest;
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
	InstantiationManagerTest.class,
	DefaultTermConverterTest.class,
	LogicUtilTest.class,
	JpcTermWriterTest.class
	})
public class JpcTestSuite {}
