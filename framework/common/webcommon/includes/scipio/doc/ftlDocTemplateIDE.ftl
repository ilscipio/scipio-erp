<#--
* Scipio FTL doc formatting template - IDE utility HTML
*
* Produces rudimentary Strings that can be used inside of Java classes
*
* DEV NOTE: PLEASE COMMENT rather than delete.
* DEV NOTE: Nothing active is using this template anymore.
*     See the *Website.ftl and *Ofbiz.ftl variants.
*
* Invoke by running
*   ant docs-ftl-scipio-ide
* OR
*   ant docs-ftl-scipio-ide-ftlonly (no java build)
* in root project folder.
* Output folder:
*   /doc/scipio/templating/ftl/lib
*
* See
*   com.ilscipio.scipio.ce.webapp.ftl.doc.ScipioLibFtlDocFileParser
* class for data model details and
*   com.ilscipio.scipio.ce.webapp.ftl.doc.ScipioLibTemplateHelper
* for available tmplHelper methods.
*
* WARN: NO Ofbiz libraries are available here; treat this as separate project.
*
* WARN: Input documentation formatting (parsing) is sensitive to whitespace, presence and number of asterisks (*),
*    and line endings. Must be spaces-only and LF-only line endings.
*
* NOTES:
* * Documentation formatting is sensitive to whitespace, presence and number of asterisks (*),
*   and line endings. Must be spaces-only and LF-only line endings.
*   * In parser, bullets will create HTML lists, text encloding in two bullets (* *) will create
*     a heading, notes with uppercase label and "?:" or ":" prefix will be highlighted (support multi-
*     line by indenting the next line), and indents alone will create preformatted text (usually code).
*   * Small code bits may be surrounded by three curly brackets (like Trac tickets) to delineate code, {{{like this}}}.
*     Indents may be enough for other cases (but indents don't identify as code in HTML). The three curly brackets will also prevent auto-linking.
*   * To prevent auto-linking or other textual formatting (but not structural formatting), wrap in three parenthesis, (((like this))).
*   * To prevent textual formatting AND escaping, wrap in three brackets, [[[<b>like this</b>]]].
*     This can be used to inject bits of raw HTML.
*     NOTE: This is a textual operator, so it is slave to structure and currently can't (and shouldn't) be used to make complex structure.
*   * Supports limited '''bold''', ''italic'', __underline__ in paragraphs (but not labels)
-->


<#if (libFormat!"") == "scipio-lib">

<#--
*************************************
* SETTINGS *
*************************************
-->

<#-- DEV NOTE: Currently this is targeting only "templating" docs.
    There is some "theming" stuff still present that there's no easy mechanism to remove.
    Am also leaving "IMPL NOTE" comments in for the time being. -->

<#-- NOTE: currently targeting "templating" docs only -->
<#assign docPurposes = ["templating", "theming", "dev"]>
<#if !docPurpose?has_content || !docPurposes?seq_contains(docPurpose)>
  <#global docPurpose = "dev">
</#if>

<#-- Uncomment this to see all:
<#global docPurpose = "dev">-->

<#-- Maps docPurpose to pat. This matches against the label part of the note, exluding "?" and ":" -->
<#assign notesToOmitPats ={
  "templating" : "(DEV\\s+NOTES?|FIXME|TODO)",
  "theming" : "(DEV\\s+NOTES?|FIXME|TODO)",
  "dev" : false
}>

<#macro compress_single_line>
     <#local captured><#nested></#local><#t>
        ${ captured?replace("^\\s+|\\s+$|\\n|\\r", "", "rm") }<#t>
</#macro>
<#macro sanitize>
    <#local captured><#nested></#local>
        ${ captured?replace("\"", "'") }
</#macro>
<#macro code_compress>
    <#local captured><#nested></#local>
    ${ captured?replace("\\n|\\r", "<br/>", "rm") }
</#macro>
<#--
*************************************
* INCLUDES *
*************************************
-->

<#include "ftlDocStdLibEmu.ftl">
<#include "ftlDocCommon.ftl">

<#--
*************************************
* HEAD *
*************************************
-->
<#--
*************************************
* MAIN PAGE *
*************************************
-->

  <#--  <div>
      <#if pageTitle?has_content>
        <h1 ><@labelText text=pageTitle /></h1>
      </#if>
      <#if docPurpose == "templating">
        <h2>Templating Documentation</h2>
      <#elseif docPurpose == "theming">
        <h2>Theming Documentation</h2>
      <#else>
        <h2>Dev Documentation</h2>
      </#if>
      <#if libFilename?has_content>
        <p><em><span >${escapeText(libFilename)}</span></em></p>
      </#if>

      <#if pageDesc?has_content>
        <div >
        <@contentGroup>
          <#list tmplHelper.splitToParagraphs(pageDesc) as part>
            <@complexContent text=part paragraphs=true/>
          </#list>
        </@contentGroup>
        </div>
      </#if>
    </div>

  <#if (sectionMap?size >= 2) && entryMap?has_content>
    <div >
      <h2>Sections:</h2>
      <ul>
        <#list sectionMap?keys as sectionName>
          <#assign section = sectionMap[sectionName]>
          <#if section.entryMap?has_content>
            <li><a href="#section-${sectionName}"><#rt>
                <#if section.title?has_content><@labelText text=section.title /><#t>
                <#elseif sectionName == "default"><@labelText text="MAIN" /><#t>
                <#else><@labelText text=sectionName /></#if><#t>
                </a>
                <p>
                <#list section.entryMap?keys?sort as entryName>
                  <a href="#entry-${entryName}">${entryName}</a><#if entryName_has_next>, </#if>
                </#list>
                </p>
            </li><#lt>
          </#if>
        </#list>
      </ul>
    </div>
  </#if>

  <#if entryMap?has_content>
    <div >
      <h2><#if (sectionMap?size >= 2)>All </#if>Definitions:</h2>
      <p>
      <#list entryMap?keys?sort as entryName>
        <a href="#entry-${entryName}">${entryName}</a><#if entryName_has_next>, </#if>
      </#list>
      </p>
    </div>
  </#if>

  <#if libMap?has_content>
    <div >
      <h2>All libraries:</h2>
      <p>
      <#list libMap?keys?sort as libName>
        <#assign lib = libMap[libName]>
        <a href="${makeInterLibUrl(lib.libDocPath)}">${escapeText(libName)}</a><#if libName_has_next>, </#if>
      </#list>
      </p>
    </div>
  </#if>
    -->

    <#-- NOTE: there is a global entryMap, and each section also has its own entryMap -->

<#-- Needs special handling -->
<#macro parametersSection entry entrySection={}>
    <#if !parametersSectionRendered>
        <div >
            <em><@labelText text=entrySection.title!"Parameters" /></em>:<br/>
      <#if false>
            <div ><#--<em>All parameters:</em>-->
            <p>
                                  <#-- NOTE: do not sort -->
            <#if entry.argList?has_content>
                <#list entry.argList as argName>
                    <code>${escapeText(argName)}</code><#if argName_has_next>, </#if>
                </#list>
                <#else>
                                    (none)
                                  </#if>
            </p>
                                </div>
                                <br/><br/>
                              </#if>
        <#-- NOTE: there is an entry-wide paramDescMap, and each param section has one too -->
        <#--<#if entrySection.paramDescMap?has_content>-->
        <div >
        <#assign exclude = ["(other)"]>
        <#assign defaultDesc = (entry.paramDescMap["(other)"])!"">
        <#if entry.type == "function" && !entry.isAdvancedArgs>
            <#-- If it's a non-advanced function, the param order is pretty important,
            so don't support sections and make sure argList order is followed and complete -->
            <@parametersTable paramDescMaps=entry.argMapWithParamDescMapEntries exclude=exclude defaultDesc=defaultDesc/>
            <#else>
            <#-- for macros and other: append unlisted params at end (in Other section if there were sections) -->
            <#if entrySection.paramSectionMap?has_content && (entrySection.paramSectionMap?size >= 2)>
                <#list entrySection.paramSectionMap?keys as paramSectionName>
                    <#assign paramSection = entrySection.paramSectionMap[paramSectionName]>
                    <div >
                                          <em><@labelText text=paramSection.title /></em>
                                          <@parametersTable paramDescMaps=paramSection.paramDescMap exclude=exclude defaultDesc=defaultDesc />
                    </div>
                                      </#list>
                <#--
                <#if entry.argMapUnaccounted?has_content>
                <div >
                <h5><@labelText text="Other" /></h5>
                <@parametersTable paramDescMaps=entry.argMapUnaccounted exclude=exclude defaultDesc=defaultDesc />
                </div>
                </#if>-->
                <#else>
                <@parametersTable paramDescMaps=entry.paramDescMapPlusArgMapUnaccounted exclude=exclude defaultDesc=defaultDesc />
            </#if>
        </#if>
        </div><br/><br/>
        <#--</#if>-->

    <#if entry.isAdvancedArgs>
        <div >
            <p>
            <#if entry.type == "function">
            <strong>NOTE:</strong> This function implements an advanced arguments interface emulating named
                parameters using maps, and the parameters above may not be positional. See <a href="https://www.scipioerp.com/community/developer/freemarker-macros/">standard/htmlTemplate</a> for details.
            <#elseif entry.type == "macro">
            <strong>NOTE:</strong> This macro implements an advanced arguments interface supplementing
                regular macro invocations. See <a href="https://www.scipioerp.com/community/developer/freemarker-macros/">standard/htmlTemplate</a> for details.
            </#if>
        </p>
        </div>
    </#if>
        </div>
    <br/><br/>
    </#if>
</#macro>

<#macro parametersTable paramDescMaps exclude=[] defaultDesc={}>
    <table >
    <#if !paramDescMaps?is_sequence>
        <#local paramDescMaps = [paramDescMaps]>
    </#if>
    <#list paramDescMaps as paramDescMap>
        <#if paramDescMap?has_content>
            <#list paramDescMap?keys as paramName>
                <#if !exclude?seq_contains(paramName)>
                    <#assign paramDesc = paramDescMap[paramName]!"">
                    <tr>
                        <td >
                            <code>${escapeText(paramName)}</code>
                        </td>
                        <td >
                      <#-- TODO? the type str can be highlighted by is currently not parsed -->
                    <@contentGroup>
                        <#if paramDesc?has_content || defaultDesc?has_content>
                            <#if !paramDesc?has_content>
                                <#local paramDesc = defaultDesc>
                            </#if>
                            <#if paramDesc.typeStr?has_content>
                                <span ><code>${escapeText(paramDesc.typeStr!"")}</code></span>
                                  </#if>
                            <#if paramDesc.shortDesc?has_content>
                                <span ><@descText text=paramDesc.shortDesc!"" /></span>
                                  </#if>
                            <#if paramDesc.extraDesc?has_content>
                                <br/>
                                    <span ><@complexContent text=paramDesc.extraDesc!"" paragraphs=false/></span>
                                  </#if>
                        </#if>
                    </@contentGroup>
                    </td>
                              </tr>
            </#if>
            </#list>
        </#if>
    </#list>
    </table>
</#macro>

<#macro parametersMap entry entrySection={}>
        <#assign exclude = ["(other)"]>
        <#assign defaultDesc = (entry.paramDescMap["(other)"])!"">
        <#if entry.type == "function" && !entry.isAdvancedArgs>
            <@parametersTableMap paramDescMaps=entry.argMapWithParamDescMapEntries exclude=exclude defaultDesc=defaultDesc/>
            <#else>
            <#if entrySection.paramSectionMap?has_content && (entrySection.paramSectionMap?size >= 2)>
                <#--<#list entrySection.paramSectionMap?keys as paramSectionName>
                    <#assign paramSection = entrySection.paramSectionMap[paramSectionName]>
                    <@parametersTableMap paramDescMaps=paramSection.paramDescMap exclude=exclude defaultDesc=defaultDesc />
                </#list>-->null);
            <#else>
                <@parametersTableMap paramDescMaps=entry.paramDescMapPlusArgMapUnaccounted exclude=exclude defaultDesc=defaultDesc/>
            </#if>
        </#if>
</#macro>

<#macro parametersTableMap paramDescMaps exclude=[] defaultDesc={}>
    <#if !paramDescMaps?is_sequence>
        <#local paramDescMaps = [paramDescMaps]>
    </#if>
    <#list paramDescMaps as paramDescMap>
        <#if paramDescMap?has_content>
            new HashMap(){{
            <#list paramDescMap?keys as paramName>
                <#if !exclude?seq_contains(paramName)>
                    <#assign paramDesc = paramDescMap[paramName]!"">
                    put("${escapeText(paramName)}", new HashMap(){{
                        <#if paramDesc?has_content || defaultDesc?has_content>
                            <#if !paramDesc?has_content>
                                put("desc","<@compress_single_line><@sanitize><#local paramDesc = defaultDesc></@sanitize></@compress_single_line>");
                            </#if>
                            <#if paramDesc.typeStr?has_content>
                                put("code","<@compress_single_line><@sanitize><code>${escapeText(paramDesc.typeStr!"")}</code></@sanitize></@compress_single_line>");
                            </#if>
                            <#if paramDesc.shortDesc?has_content>
                                 put("shortdesc","<@compress_single_line><@sanitize><@descText text=paramDesc.shortDesc!"" /></@sanitize></@compress_single_line>");
                            </#if>
                            <#if paramDesc.extraDesc?has_content>
                                put("extraDesc","<@compress_single_line><@sanitize><@complexContent text=paramDesc.extraDesc!"" paragraphs=false/></@sanitize></@compress_single_line>");
                            </#if>
                        </#if>

                    }}
                    );
                </#if>
            </#list>
            }});
        </#if>
    </#list>
</#macro>


  <#list sectionMap?keys as sectionName>
    <#assign section = sectionMap[sectionName]>
    <#-- print "none" below instead
    <#if section.entryMap?has_content>-->
    <#--
    <#if section.title?has_content>
      <hr />
    </#if>
    -->
    <#if section.entryMap?has_content || sectionName != "default">
      <#--<a name="section-${sectionName}"></a>-->
        <#--
        <#if section.title?has_content>
          <@labelText text=section.title />
        </#if>
        <#if section.comment?has_content>
          <@complexContent text=section.comment />
        </#if>-->
      <#if section.entryMap?has_content>
        <#list section.entryMap?keys as entryName>
            <#assign entry = section.entryMap[entryName]>
            <#if entry.isImplemented>
                <#assign entryTitle = entry.title!"">
                <#if !entryTitle?has_content>
                    <#assign entryTitle = entryName>
                </#if>
                <#global parametersSectionRendered = false>
    public static Map ${entry.type?upper_case}_${entryTitle?upper_case?replace(" ","_")?replace("(","")?replace(")","")} = new HashMap(){{
        put("title","<@compress_single_line><@labelText text=entryTitle /></@compress_single_line>");
        put("type","<@compress_single_line>${entry.type}</@compress_single_line>");
        put("code","<@compress_single_line><@sanitize><code>${entryName}</code></@sanitize></@compress_single_line>");
        put("warning","<@compress_single_line><#if entry.isAbstract>(abstract/placeholder)</#if><#if entry.isDeprecated> <strong>(DEPRECATED)</strong></#if><#if entry.isOverride> <strong>(override)</strong></#if></@compress_single_line>");
        <#if entry.isTransform || (entry.isAdvancedArgs!false)><@compress_single_line>put("transform","
                <p><#if entry.isTransform><span >Implemented as transform.</span></#if></p>
            </@compress_single_line>");</#if>
        <#if libFilename?has_content>put("infoLink","<@compress_single_line><@sanitize>
            <#if escapeText(libFilename) == "standard/htmlStructure.ftl">
                <a href="https://www.scipioerp.com/community/developer/freemarker-macros/htmlstructure/#entry-${entryName}">${entryName}</a>
                        </#if>
            <#if escapeText(libFilename) == "standard/htmlContent.ftl">
                <a href="https://www.scipioerp.com/community/developer/freemarker-macros/htmlcontent/#entry-${entryName}">${entryName}</a>
                        </#if>
            <#if escapeText(libFilename) == "standard/htmlInfo.ftl">
                <a href="https://www.scipioerp.com/community/developer/freemarker-macros/htmlinfo/#entry-${entryName}">${entryName}</a>
                        </#if>
            <#if escapeText(libFilename) == "standard/htmlForm.ftl">
                <a href="https://www.scipioerp.com/community/developer/freemarker-macros/htmlform/#entry-${entryName}">${entryName}</a>
                        </#if>
            <#if escapeText(libFilename) == "standard/htmlNav.ftl">
                <a href="https://www.scipioerp.com/community/developer/freemarker-macros/htmlnav/#entry-${entryName}">${entryName}</a>
                        </#if>
            <#if escapeText(libFilename) == "standard/htmlScript.ftl">
                <a href="https://www.scipioerp.com/community/developer/freemarker-macros/htmlscript/#entry-${entryName}">${entryName}</a>
                        </#if>
            <#if escapeText(libFilename) == "utilities.ftl">
                <a href="https://www.scipioerp.com/community/developer/freemarker-macros/utilities/#entry-${entryName}">${entryName}</a>
                        </#if>
        </@sanitize>");</@compress_single_line></#if>
        <#assign entrySections = entry.sections>
        <#list entrySections?keys as entrySectionName>
            <#assign entrySection = entrySections[entrySectionName]>
            <#if entrySectionName == "mainDesc">
        put("desc","<@code_compress><@sanitize>
        <@contentGroup>
            <#if (entrySection.shortDesc)?has_content>
                <p ><@descText text=entrySection.shortDesc /></p>
        </#if>
            <#if (entrySection.extraDesc)?has_content>
                <#list tmplHelper.splitToParagraphs(entrySection.extraDesc) as part>
                    <@complexContent text=part paragraphs=true />
                </#list>
            </#if>
        </@contentGroup>
        </@sanitize>");</@code_compress>
            <#elseif entrySectionName == "examples"><#t>
        put("examples","<@compress_single_line><@sanitize><h2><@labelText text=entrySection.title!"" /></h2><@code_compress><@preformedText text=entrySection.rawText!"" /></@code_compress></@sanitize>");</@compress_single_line>
            <#elseif entrySectionName == "parameters"><#t>
        put("parameters","<@compress_single_line><@sanitize><@parametersSection entry=entry entrySection=entrySection /></@sanitize>");</@compress_single_line>
        put("parametersMap",<@parametersMap entry=entry entrySection=entrySection />
            <#elseif entrySectionName == "returnValues"><#t>
        put("return","<@compress_single_line><@sanitize><@parametersSection entry=entry entrySection=entrySections.parameters!{} />
            <div >
                <h2><@labelText text=entrySection.title!"" /></h2>
                <p><#t>
                  <@descText text=entrySection.text!"" />
            </p><#t>
              </div></@sanitize>");</@compress_single_line>
           <#elseif entrySectionName == "related">
        put("related","<@compress_single_line><@sanitize><@parametersSection entry=entry entrySection=entrySections.parameters!{} /></@sanitize>");</@compress_single_line>
            <#--<h2><@labelText text=entrySection.title!"" /></h2>
            <p>
            <#list entry.relatedNames![] as name>
            <@entryRef name=name /><#if name_has_next>, </#if>
            </#list>
            </p>
            -->
            <#else>
            <#--<@parametersSection entry=entry entrySection=entrySections.parameters!{} /> Don't force parameters before others -->
        put("extraInfo","<@compress_single_line><@sanitize>
        <h2><@labelText text=entrySection.title!"" /></h2>
        <@complexContent text=entrySection.text!"" paragraphs=true />
        </@sanitize>");</@compress_single_line>
            </#if>

        </#list>
        <#--<@parametersSection entrySection=entrySections.parameters!{} entry=entry />-->
      }};
        </#if>

        </#list>
      <#else>
          <#--<p><em>(No public definitions in this section)</em></p>-->
      </#if>
    </#if>
  </#list>
<#else>
  <head><title>ERROR</title></head>
  <body><p>ERROR: This doc template can only render scipio-lib format FTL lib docs.</p></body>
</#if>
