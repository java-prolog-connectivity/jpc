package org.jpc.internal.regex;

import static java.util.Arrays.asList;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.List;

import org.junit.Test;

public class RegExUtilTest {

	@Test
	public void testHasExtension() {
		List<String> extensions = asList("lgt", "pl", "P");
		assertFalse(RegExUtil.hasExtension("hello.lgt"));
		assertTrue(RegExUtil.hasExtension("hello.lgt", extensions));
		assertTrue(RegExUtil.hasExtension(".lgt", extensions));
		assertFalse(RegExUtil.hasExtension("lgt", extensions));
		assertFalse(RegExUtil.hasExtension("hello.lgt.x", extensions));
		assertFalse(RegExUtil.hasExtension("hello.lgtx", extensions));
		assertTrue(RegExUtil.hasExtension("hello.pl", extensions));
		assertTrue(RegExUtil.hasExtension("hello.P", extensions));
		assertFalse(RegExUtil.hasExtension("hello.p", extensions));
	}
}
