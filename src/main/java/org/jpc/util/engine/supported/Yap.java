package org.jpc.util.engine.supported;

public class Yap extends EngineDescription {

	public static final String YAP = "YAP";
	public static final String YAP_DIALECT = YAP.toLowerCase();
	public static final String YAP_BIN_DIRECTORY_PROPERTY_NAME = "YAP_BIN_DIRECTORY";
	private static final String EXECUTABLE_FILE_NAME = "yap";
	
	@Override
	public String getName() {
		return YAP;
	}

	@Override
	public String getDialect() {
		return YAP_DIALECT;
	}
	
	public String getExecutableFileName() {
		return EXECUTABLE_FILE_NAME;
	}
	
	@Override
	public String getDescription() {
		return "YAP is a high-performance Prolog compiler developed at LIACC/Universidade do Porto and at COPPE Sistemas/UFRJ." + 
				"Its Prolog engine is based in the WAM (Warren Abstract Machine), with several optimizations for better performance.";
	}

	@Override
	public String getLicenseUrl() {
		return "http://www.dcc.fc.up.pt/~vsc/Yap/Artistic";
	}

	@Override
	public String getSiteUrl() {
		return "http://www.dcc.fc.up.pt/~vsc/Yap/";
	}
	
}
