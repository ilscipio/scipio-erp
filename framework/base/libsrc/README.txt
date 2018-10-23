SCIPIO: This folder is for ivy-downloaded source JARs (*-sources.jar).

Can be populated from project root using:

  ./ant init-lib-update-withsources
  ./ant init-lib-update-withsources-force
  
The former can fail to detect changes or lack of source JARs,
hence you may need to use the latter.

To download source jars during build:

  ./ant build -Dlib.update.sources=true
  ./ant build -Dlib.update.sources=true -Dlib.update.force=true
