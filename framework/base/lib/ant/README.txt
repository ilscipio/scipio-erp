SCIPIO: 2017-02-02: 
The ant launcher, library and junit extension have been moved from base/lib to
under base/lib/ant.

The effective and desired result is that the other JAR libraries under base/lib
are no longer automatically on the classpath.

This primarily affects junit and ant <junit/> directives, which must now
explicitly give the path to the junit(-dep)-*.jar file one way or another.
