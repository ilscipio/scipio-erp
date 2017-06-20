SCIPIO: This folder holds cache data, temporary files, and potentially other files needed
for the Apache Ivy external library dependency management resolver currently in use 
by Scipio CE since v1.14.3 (main).

For cache configuration and layout, please see ivysettings.xml in the root project directory.

Note that there are two different caches for Ivy, a repository cache and a resolution cache.
In the Scipio Git repositories, by default these are configured to use two different locations:
* repository cache: user home directory ~/.ivy2/* (default Ivy location)
* resolution cache: [project-root]/ivy/resolutionCache/* (project-local)
This provides multi-project development efficiency and speed while preventing conflicts between projects.
An alternate configuration for project-local repository caching is provided and may be set in ivysettings.xml.
