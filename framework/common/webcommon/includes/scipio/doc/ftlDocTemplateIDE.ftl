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
<#assign notesToOmitPats = {
  <#--"templating" : "(DEV\\s+NOTES?|IMPL\\s+NOTES?|IMPLEMENTATION\\s+NOTES?|FIXME|TODO)",-->
  "templating" : "(DEV\\s+NOTES?|FIXME|TODO)",
  "theming" : "(DEV\\s+NOTES?|FIXME|TODO)",
  "dev" : false
}>

<#macro compress_single_line>
     <#local captured><#nested></#local>
        ${ captured?replace("^\\s+|\\s+$|\\n|\\r", "", "rm") }
</#macro>
<#macro sanitize>
    <#local captured><#nested></#local>
        ${ captured?replace("<a.*?</a>","","rms")?replace("\"", "'") }
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
      <h4>Sections:</h4>
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
      <h4><#if (sectionMap?size >= 2)>All </#if>Definitions:</h4>
      <p>
      <#list entryMap?keys?sort as entryName>
        <a href="#entry-${entryName}">${entryName}</a><#if entryName_has_next>, </#if>
      </#list>
      </p>
    </div>
  </#if>

  <#if libMap?has_content>
    <div >
      <h4>All libraries:</h4>
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
            <@compress_single_line>
              <#compress>
              <#assign entry = section.entryMap[entryName]>
              <#if entry.isImplemented>
                <#assign entryTitle = entry.title!"">
                <#if !entryTitle?has_content>
                  <#assign entryTitle = entryName>
                </#if>
                public static String ${entry.type?upper_case}_${entryTitle?upper_case?replace(" ","_")?replace("(","")?replace")",""} = "
                    <@sanitize>
                      <#--
                      <hr />
                      <a name="entry-${entryName}"></a>-->
                      <div >
                        <#-- NOTE: title is sometimes same as formal name (below), but not always -->
                        <h3 ><@labelText text=entryTitle /></h3>

                        <div >
                           <#-- type is "macro", "function" or "variable" -->
                           <h4><span >${entry.type}</span> <span ><code>${entryName}</code></span><#if entry.isAbstract> (abstract/placeholder)</#if>
                              <#if entry.isDeprecated> <strong>(DEPRECATED)</strong></#if><#if entry.isOverride> <strong>(override)</strong></#if></h4>
                        </div>

                        <#global parametersSectionRendered = false>

                        <#-- Needs special handling -->
                        <#macro parametersSection entry entrySection={}>
                            <#if !parametersSectionRendered>
                              <div >
                                <h4><@labelText text=entrySection.title!"Parameters" /></h4>

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
                                          <h5><@labelText text=paramSection.title /></h5>
                                          <@parametersTable paramDescMaps=paramSection.paramDescMap exclude=exclude defaultDesc=defaultDesc />
                                        </div>
                                      </#list>
                                      <#if entry.argMapUnaccounted?has_content>
                                    <div >
                                      <h5><@labelText text="Other" /></h5>
                                      <@parametersTable paramDescMaps=entry.argMapUnaccounted exclude=exclude defaultDesc=defaultDesc />
                                    </div>
                                  </#if>
                                <#else>
                                  <@parametersTable paramDescMaps=entry.paramDescMapPlusArgMapUnaccounted exclude=exclude defaultDesc=defaultDesc/>
                                </#if>
                              </#if>
                              </div>
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
                        </#if>
                        <#global parametersSectionRendered = true>
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

                    <#assign entrySections = entry.sections>
                    <#list entrySections?keys as entrySectionName>
                      <#assign entrySection = entrySections[entrySectionName]>

                        <#if entrySectionName == "mainDesc">
                          <div >
                          <@contentGroup>
                            <#if (entrySection.shortDesc)?has_content>
                              <p ><@descText text=entrySection.shortDesc /></p>
                            </#if>
                            <#if (entrySection.extraDesc)?has_content>
                              <div >
                                <#list tmplHelper.splitToParagraphs(entrySection.extraDesc) as part>
                                  <@complexContent text=part paragraphs=true />
                                </#list>
                              </div>
                            </#if>
                          </@contentGroup>
                          </div>
                        <#elseif entrySectionName == "examples">
                          <div >
                            <h4><@labelText text=entrySection.title!"" /></h4>
                            <@preformedText text=entrySection.rawText!"" />
                          </div>
                        <#elseif entrySectionName == "parameters">
                          <@parametersSection entry=entry entrySection=entrySection />
                        <#elseif entrySectionName == "returnValues">
                          <@parametersSection entry=entry entrySection=entrySections.parameters!{} /><#-- Guarantee parameters before return values -->
                          <div >
                            <h4><@labelText text=entrySection.title!"" /></h4>
                            <p>
                              <@descText text=entrySection.text!"" />
                            </p>
                          </div>
                        <#elseif entrySectionName == "related">
                          <@parametersSection entry=entry entrySection=entrySections.parameters!{} /><#-- Guarantee parameters before related -->
                          <div >
                             <h4><@labelText text=entrySection.title!"" /></h4>
                             <p>
                             <#list entry.relatedNames![] as name>
                               <@entryRef name=name /><#if name_has_next>, </#if>
                             </#list>
                             </p>
                          </div>
                        <#else>
                          <#--<@parametersSection entry=entry entrySection=entrySections.parameters!{} /> Don't force parameters before others -->
                          <div >
                            <h4><@labelText text=entrySection.title!"" /></h4>
                            <@complexContent text=entrySection.text!"" paragraphs=true />
                          </div>
                        </#if>

                    </#list>
                    <@parametersSection entrySection=entrySections.parameters!{} entry=entry />

                    <#if entry.isTransform || (entry.isAdvancedArgs!false)>
                      <div >
                        <p>
                          <#if entry.isTransform><span >Implemented as transform.</span></#if>
                        </p>
                      </div>
                    </#if>

                  </div>
                  </@sanitize>";
          </#if>
          </#compress>
          </@compress_single_line>
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
