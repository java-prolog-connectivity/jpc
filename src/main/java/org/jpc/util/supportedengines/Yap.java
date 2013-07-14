package org.jpc.util.supportedengines;

public class Yap extends EngineDescription {

	public static final String YAP = "YAP";
	
	@Override
	public String getName() {
		return YAP;
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
