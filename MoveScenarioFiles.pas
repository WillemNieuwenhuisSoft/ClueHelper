unit MoveScenarioFiles;

interface

type
//	class ScenarioFileFilter implements FilenameFilter {
//		Pattern pattern = Pattern.compile("cov_all[.]\\d+|prob\\d+_.*[.]\\d+");
//
//		public boolean accept(File dir, String name) {
//			File ft = new File(dir.getAbsolutePath() + File.separatorChar + name);
//			if (ft.isFile())
//				return pattern.matcher(name).matches();
//
//			return false;
//		}
//
//	}
//
    MoveScenarioFiles = class

//	static Logger logger = Logger.getLogger(MoveScenarioFiles.class.getName());
//	private CluesConfig config;

	public MoveScenarioFiles(Logger logger) {
		MoveScenarioFiles.logger = logger;
	}

	public CluesConfig getConfig() {
		return config;
	}

	public void setConfig(CluesConfig config) {
		this.config = config;
	}

	private boolean createFolder(File newFolder) {
		if (newFolder.exists() && newFolder.isDirectory()) {
			logger.info("Folder '" + newFolder + "' already exist, content (if any) will be replaced");
		} else {
			boolean dirOK = newFolder.mkdir();
			if (!dirOK) {
				logger.severe("Unable to create base folder '" + newFolder + "', quitting program");
				return false;
			}
		}
		return true;
	}

	public void moveFiles() {
		logger.info("Start moving files");
		File source = new File(config.getSourcePath());
		if (!source.isDirectory()) {
			logger.severe("Source path is not a folder, abort operation");
			return;
		}
		File dest = new File(config.getDestPath());
		if (!createFolder(dest)) {
			logger.severe("Unable to create destination folder; moving files NOT initiated");
			return;
		}

		logger.info(source + " --> " + dest);
		String[] files = source.list(new ScenarioFileFilter());
		int done = 0;
		for (String f : files) {
			File file = new File(source + File.separator + f);
			if (file.isFile()) {
				Filename fname = Filename.parse(file.getAbsolutePath());
				fname.setPath(dest.getAbsolutePath());
				fname.setName(fname.getNameExt());	// change name into name + ext (all files have the same name part)
				fname.setExt(".asc");
				File newFile = new File(fname.toString());
				boolean ok = file.renameTo(newFile);	// move and rename
				if (!ok)
					logger.warning(file + " NOT moved to " + newFile);
				else
					done++;
			}
		}
		logger.info("Moving files complete; " + Integer.toString(done) + " files moved");
	}

}

implementation

end.
