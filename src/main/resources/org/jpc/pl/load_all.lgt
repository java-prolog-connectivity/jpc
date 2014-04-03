:- initialization((
	%set_logtalk_flag(report, off),
	set_logtalk_flag(optimize, on),
	logtalk_load_context(directory, Directory),
	assertz(logtalk_library_path(jpc_core, Directory)),
	logtalk_load([atom_term_util,javap])
)).