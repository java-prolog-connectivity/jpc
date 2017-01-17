package org.jpc;

import org.jpc.converter.CustomTermConversionsTest;
import org.jpc.converter.DefaultConversionsTest;
import org.jpc.converter.catalog.CustomTermToObjectConversionsTest;
import org.jpc.converter.catalog.TypedTermConversionsTest;
import org.jpc.converter.catalog.error.ErrorConversionsTest;
import org.jpc.converter.catalog.io.FileConversionsTest;
import org.jpc.converter.catalog.reflection.ConstructorConversionTest;
import org.jpc.converter.catalog.reflection.FieldResolutionConversionsTest;
import org.jpc.converter.catalog.reflection.MethodCallConversionsTest;
import org.jpc.converter.catalog.reflection.StaticClassConversionsTest;
import org.jpc.converter.catalog.reflection.reification.TypeConversionsTest;
import org.jpc.converter.typesolver.TypeSolverTest;
import org.jpc.engine.embedded.JpcEmbeddedEngineTestSuite;
import org.jpc.internal.reflection.ReflectionUtilTest;
import org.jpc.internal.regex.RegExUtilTest;
import org.jpc.salt.JpcTermWriterTest;
import org.jpc.term.AbstractTermTest;
import org.jpc.term.AtomTest;
import org.jpc.term.CompoundTest;
import org.jpc.term.FloatTermTest;
import org.jpc.term.HilogTermTest;
import org.jpc.term.IntegerTermTest;
import org.jpc.term.JRefTest;
import org.jpc.term.ListTermTest;
import org.jpc.term.RefTermTest;
import org.jpc.term.SerializedTermTest;
import org.jpc.term.VariableTest;
import org.jpc.term.expansion.ParameterizedSymbolExpanderTest;
import org.jpc.term.unification.UnificationTest;
import org.jpc.util.TermJoinerTest;
import org.jpc.util.config.EngineConfigurationDeserializerTest;
import org.jpc.util.config.JpcConfigurationDeserializerTest;
import org.jpc.util.reification.ReflectiveClassTest;
import org.jpc.util.reification.ReflectiveObjectTest;
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
	RefTermTest.class,
	SerializedTermTest.class,
	TypeSolverTest.class,
	DefaultConversionsTest.class,
	CustomTermConversionsTest.class,
	TypeConversionsTest.class,
	StaticClassConversionsTest.class,
	TypedTermConversionsTest.class,
	CustomTermToObjectConversionsTest.class,
	ConstructorConversionTest.class,
	MethodCallConversionsTest.class,
	FieldResolutionConversionsTest.class,
	FileConversionsTest.class,
	ErrorConversionsTest.class,
	TermJoinerTest.class,
	EngineConfigurationDeserializerTest.class,
	JpcConfigurationDeserializerTest.class,
	ReflectiveClassTest.class,
	ReflectiveObjectTest.class,
	ReflectionUtilTest.class,
	RegExUtilTest.class
})
public class JpcTestSuite {}
