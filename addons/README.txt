SCIPIO:
This is the official, general addons directory.

It is intended for any extra projects or modules, which may or may not follow 
Ofbiz component structure or integrate with Ofbiz in its traditional way.

LOADABLE COMPONENTS

Each folder may contain the following traditional component files:

  build.xml
  scipio-component.xml/scipio-theme.xml/ofbiz-component.xml

Folders which contain a build.xml (Ant) file are treated as Ofbiz components for the 
build process. Such build.xml files should import the common.xml file found
in the root SCIPIO project directory (or build will fail). Build order follows the same rules
as hot-deploy (by default, arbitrary, but supports an explicit build.xml file
directly in addons directory).

Folders which contain a scipio-component.xml/scipio-theme.xml/ofbiz-component.xml file are treated as Scipio components
for runtime loading. Build order follows the same rules as hot-deploy (by default,
arbitrary, but supports an explicit component-load.xml file directly in addons
directory).

Folders which do not contain a build.xml or scipio-component.xml/scipio-theme.xml/ofbiz-component.xml files are
not considered components and may follow any other layout (with other exceptions below).

BUILD HOOKS AND EXTENSIONS

Each folder may also contain the following special SCIPIO-only Ant files:

  build-hooks.xml - build process hooks (see root build.xml for available hooks)
  build-main.xml - main build additional definitions (extends root build.xml)
  build-component-common.xml - common component build additions (extends root common.xml)

These files can used to implement hooks into the main build process and
provide additional definitions to the main and common build files.
Note that extension files such as build-main.xml and build-component-common.xml cannot
override existing targets, but they may define new ones (due to Ant behavior).

NOTE: Build hooks and extensions are also supported for hot-deploy components.

