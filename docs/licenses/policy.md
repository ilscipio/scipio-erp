# Third-party license policy

Work packages L-06 and L-06b of the blueprint (`scipio-apps/docs/20-blueprint.md`,
section 12, license track). Date 2026-09-30. This file is the core copy of the
policy of `scipio-ai` (`docs/licenses/policy.md`); the two policies must agree. Owner of the policy: lane A. The
legal review of the texts is L-05 (owner and lawyer).

Under decision D14, the Scipio 4.0 core is AGPL-3.0 plus a commercial
license, and `scipio-ai` is closed. In this repository, all code is core. This policy puts each third-party license
into one of three classes: allowed, review, blocked. The build task
`checkLicensePolicy` applies it. The data for the task is
`gradle/license-policy.json`. Change this file and the data file together.

## Scope

The policy applies to these parts:

- The jars that `syncLibs` copies to `framework/base/lib/gradle`: the runtime
  classpath of all components (the configuration `runtimeLibs`).
- The jars that `syncSolrWebappLibs` copies to the Solr webapp (the
  configuration `solrWebapp` of `:applications:solr`).
- The jars that Git tracks (for example `framework/base/lib/ant/` and
  `ivy/localRepo/`).
- The JavaScript and CSS libraries that Git tracks, mainly in `themes/`,
  `framework/images/` and `applications/solr/webapp/libs/`.

The core must stay free for the commercial license, so the classes are those
of the AGPL core.

## Classes

### Allowed

The check lets these licenses through. Keep the copyright notices and the
license texts. For Apache-2.0, keep the `NOTICE` texts too.

| License (SPDX) | Reason |
|---|---|
| Apache-2.0, MIT, MIT-0, BSD-2-Clause, BSD-3-Clause, ISC, 0BSD, Zlib, BSL-1.0, PostgreSQL, UPL-1.0, W3C, Unicode-3.0, NetCDF, OGC-1.0, WTFPL | Permissive licenses. The FSF lists them (or their base texts) as compatible with the GPL, so also with the AGPL-3.0. They put no copyleft on the combined work, so the commercial license of the core stays possible. |
| Unlicense, CC0-1.0, LicenseRef-Public-Domain | No conditions. |
| LicenseRef-Ilscipio-Proprietary | Own code of Ilscipio, only for `scipio-ai`. In the core, the check blocks it (`closedOnly`): the core never depends on a closed part (blueprint 3.3). |

### Review

The check lets these licenses through and lists them in the report. Lane A
decides each one, with the lawyer when necessary (L-05). A review license is
not a failure of the build.

| License (SPDX) | Reason |
|---|---|
| CDDL-1.0, CDDL-1.1, EPL-1.0, CPL-1.0, MPL-1.1 | Copyleft on the file level. The FSF calls these licenses incompatible with the GPL. An unchanged library jar next to the AGPL core is common, but a lawyer must confirm it for the AGPL and for the commercial license. |
| EPL-2.0 | Compatible with the GPL only when the library names the GPL as a "Secondary License". Else, as EPL-1.0. |
| MPL-2.0 | Compatible with the GPL, but not when the library is "Incompatible With Secondary Licenses". |
| LGPL-2.1, LGPL-3.0, LGPL (no version) | Compatible with the AGPL. The commercial license must let the user replace the library and must give its source. |
| GPL-2.0-only WITH Classpath-exception-2.0 | The exception permits a link from other code. A change to the library itself stays under the GPL-2.0. |
| Apache-1.1, xpp, LicenseRef-JDOM, Plexus | Permissive, but with name or credit clauses. The FSF calls Apache-1.1 incompatible with the GPL for these clauses. The other three have clauses of the same type. |
| BSD-3-Clause-No-Nuclear-Warranty | BSD-3-Clause with an extra clause about nuclear facilities. The FSF and the OSI do not list it. |
| CC-BY, CC-BY-2.5, CC-BY-3.0, CC-BY-4.0, CC-BY-SA-3.0, CC-BY-SA-4.0 | Creative Commons did not write these licenses for code. CC-BY-SA has a share-alike rule. |

### Blocked

The check fails on a module when each of its licenses is blocked. A license
that the policy does not name is blocked.

| License (SPDX) | Reason |
|---|---|
| GPL-2.0-only | Not compatible with the AGPL-3.0: it has no "or later" clause. |
| GPL-3.0-only, GPL (no version), AGPL-3.0-only | Compatible with the AGPL-3.0, but third-party copyleft code stops the commercial license of the core. Ilscipio cannot give a commercial license for code of other owners. In `scipio-ai`, this code would open the closed layer. |
| SSPL-1.0 | Not an open-source license. Its section 13 puts the whole service stack under the SSPL. |
| BUSL-1.1 | Source-available only. It forbids some production use, for example a hosted service that competes with the licensor. |
| LicenseRef-Commons-Clause | It forbids to sell the software. Not an open-source license. |
| JSON | "The Software shall be used for Good, not Evil" is a use restriction. The FSF and Debian call the license not free. |
| LicenseRef-UnRAR | It forbids use for an archiver that is compatible with RAR. A use restriction: not free and not compatible with the AGPL. |
| CC-BY-NC | It forbids commercial use. |
| LicenseRef-Unknown | The check found no license, or a license name that no alias maps. Without a license, the owner keeps all rights. |

## Rules of the check

1. The check reads the licenses of each module from its POM, its manifest and
   its license files (the jk1 plugin). For a jar that Git tracks, it reads the
   policy entry for the path, else the manifest (`Bundle-License`), else the
   license file in the jar.
2. A web library is a folder below `bower_components`, `node_modules` or
   `libs`, or a folder with a `bower.json` or `package.json` that names a
   license. Outside these folders, a single file counts when its header names a
   license, or when it is a minified file. The license comes from the policy
   (`web`), else from the metadata file, else from a license file in the
   folder, else from the header of the first file that names one. Only comments
   in the first 3000 characters count as a header. The list `webFirstParty`
   names folders with an own `package.json` that are not libraries. The list
   `webOwn` names files that Gradle or Gulp builds from own code.
3. The `aliases` list maps each license name or URL to one SPDX id. The first
   pattern that matches wins. A name that no pattern matches becomes
   `LicenseRef-Unknown`. `aliasSamples` holds license texts and the id that
   the aliases must give. The check fails when a sample gives a different id.
4. Two or more licenses on one module are a choice (OR). The best class
   counts. The report lists these modules in the section "Allowed by a choice".
   When the licenses cover different parts of the module, add an AND entry to
   `declared`.
5. A module without a license gets `LicenseRef-Unknown`. That is blocked.
6. `declared` holds the licenses that the metadata does not give, or gives
   wrong. Each entry names its source. `jars` and `web` do the same for jars
   and web libraries.
7. `exceptions` holds the blocked dependencies that lane A lets through for a
   time. Put the version in the key. Remove the entry when the dependency
   goes. In L-06b, the list is empty.
8. `closedOnly` names the licenses that only the closed layer may use. In this
   repository, the check blocks them.

## How to change the policy

1. To add an alias or a declared license, name the source in the entry. For
   a new alias, add a sample to `aliasSamples`.
2. To move a license to a different class, get the approval of lane A. For a
   legal question, lane A asks the lawyer (L-05).
3. To add an exception, get the approval of lane A. Write the reason and the
   proposed fix into the entry.
4. Run `gradlew checkLicensePolicy` and look at the report
   `build/reports/licenses/license-report.md`.

## Limits

- The check trusts the metadata of each module. A fat jar can hold libraries
  with other licenses, and the metadata does not always show them.
- The rule "a list is a choice" can hide a part under a review or blocked
  license. The section "Allowed by a choice" of the report lists each such
  entry for a manual check.
- The web scan reads the license from metadata and headers. It does not read
  the license texts of a library in full. Entries in `web` with a source name
  are declared by hand; L-05 confirms the texts.
- The check does not cover fonts, images and other files that are not jars,
  JavaScript or CSS.
- The check does not cover the jars of an addon in `hot-deploy` or `addons`
  that a component copies into its own `lib/` folder.
