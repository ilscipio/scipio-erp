Scipio Dependency Management: Apache Ivy

Since v1.14.3, Scipio uses Apache Ivy 2 for library dependency management.
This ivy/ folder holds Ivy settings, the resolution (and optional repository/download) cache, 
and a local local/manual repository.

SETTINGS

see ivysettings.xml.

CACHES

There are two different caches for Ivy, a repository cache and a resolution cache.
In the Scipio Git repositories, by default these are configured to use two different locations:

* repository cache: user home directory ~/.ivy2/* (default Ivy location)
* resolution cache: resolutionCache/* (project-local)

This provides multi-project development efficiency and speed while preventing conflicts between projects.

An alternate configuration for project-local repository caching is provided and may be set in ivysettings.xml.

LOCAL REPOSITORY

Since 2018-03-19, a local Ivy file repository now exists here as:

* localRepo/

JARs (artifacts) can be added to this folder following the path/file naming pattern defined
in ivysettings.xml for the "local-ivy" resolver. It is treated by Ivy like any other
repository as a source of artifacts. In ivysettings.xml it is configured through the default chain resolver 
to have priority over the external (maven central) repo.

This allows adding library JARs (to stock Scipio or client projects) that are otherwise
unavailable in the public repositories, and gives a more elegant way to add any libraries
to client projects, compared to committing them directly under component lib/ folders.
This patterns helps standardize the ivy configurations and simplify the individual 
component ivy.xml, build.xml and .gitignore files.

See ivysettings.xml for the exact path/file name format for artifacts.

NOTES:
* localRepo contains modified bsh for Scipio - source available under: tools/extra/src/beanshell-2.0b6-scipio


