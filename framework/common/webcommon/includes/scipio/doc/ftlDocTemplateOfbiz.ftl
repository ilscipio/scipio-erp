<#--
* Scipio FTL doc formatting template - Dynamic Ofbiz version (for hosting within Ofbiz instance)
* 
* This contains live usage of Scipio templating API (for use within Ofbiz only).
*
* DEV NOTE: PLEASE COMMENT rather than delete.
*
* WARN: This dynamic template is EXTREMELY confusing because of the multiple 
*     Freemarker ObjectWrappers used and mixing of two libraries (ftlDocCommon.ftl and standard templating API),
*     and two data models, all with no namespaces!!! 
*     The data model for the source documents has NO auto html-escaping, but
*     all other screen values from regular Ofbiz screen context DO have auto html-escaping.
* 
* NOTE: {{{escapeVal}}} should be able to handle security in all cases,
*     at least, but it can't be used in the ftlDocCommon.ftl file right now (TODO).
* 
* See ftlDocTemplate.ftl for more information.
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


<#-- 
*************************************
* INCLUDES *
*************************************
-->

<#-- we MUST define this here, important -->
<#global baseAbsInterLibUrl = rawString(basePageIntraWebappUri)>

<#-- WARN: some these could clash with Template API macros currently, TODO checkover... -->
<#include "ftlDocCommon.ftl">

<#function makeOwaAbsInterLibUrl targetLibDocPath targetName="">
  <#return makeOfbizWebappUrl(makeAbsInterLibUrl(targetLibDocPath, targetName))><#-- NOTE: intentionally skip html escaping, never needed here. -->
</#function>

<#-- REDEFINE this to make absolute links -->
<#assign makeInterLibUrl = makeOwaAbsInterLibUrl>


<#-- 
*************************************
* HEAD *
*************************************
-->

<#-- NOTE: the styles that were initially here have been moved into each theme
    TODO: REVIEW the styles for this, in each theme -->
<style type="text/css">

<#-- FIXME: this is a hack to make page readable when no login on default theme -->
<#if !userLogin?? && (visualThemeId!) == "METRO">
#main-content {
  background-color: #F5F5F5;
}
</#if>

</style>


<#-- 
*************************************
* MAIN PAGE *
*************************************
-->  

<div class="tmpldoc-content">
  <div class="lib-page-intro">
    <#if !userLogin??><#-- FIXME: hack for no-login -->
      <h1 class="lib-pagetitle">${title}</h1>
    </#if>
   
      <#if libFilename?has_content>
        <h3><span class="lib-filename">${escapeText(libFilename)}</span></h3>
      </#if>
      
    <#-- nothing good coming from this, except maybe htmlTemplate? -->
    <#if libName == "standard/htmlTemplate">
      <#if subtitle?has_content && (rawString(subtitle)?trim != rawString(title)?trim)>
        <p><em>${escapeVal(subtitle, 'html')}</em></p>
      </#if>
    </#if>
    
      
    <div class="lib-pagetitledesc">
      <#-- duplicate
      <#if pageTitle?has_content>
        <h1 class="lib-pagetitle"><@labelText text=pageTitle /></h1>
      </#if>
      -->
      
      <div class="tmpldoc-options-menu">
        <div style="float:left;"><#-- FIXME? floats? -->
          <#-- WARN: use escapeVal on docPurpose if ever printed  -->
        <form method="get" action="<@ofbizWebappUrl uri=currPageIntraWebappUri escapeAs="html"/>">
          <@field type="select" inline=true name="docPurpose" events={"change": "jQuery(this).closest('form').submit();"}
              tooltip="Documentation Filter: Select Developer to see more implementation notes and TODOs" title="Documentation Filter">
            <@field type="option" value="templating" selected=(docPurpose == "templating")>Templating</@field>
            <#-- TODO: this makes no difference with "templating" right now
            <@field type="option" value="theming" selected=(docPurpose == "theming")>Theming</@field>-->
            <@field type="option" value="dev" selected=(!["templating", "theming"]?seq_contains(docPurpose))>Developer</@field>
          </@field>
        </form>
        </div>
        <div style="float:left; margin-left:1em;">
          <@render type="menu" resource="component://webtools/widget/Menus.xml#TemplateApiDocSubTabBar" />
        </div>
        <div style="clear:both;"></div>
      </div>

      <#if pageDesc?has_content>
        <div class="lib-pagedesc">
        <@contentGroup>
          <#list tmplHelper.splitToParagraphs(pageDesc) as part>
            <@complexContent text=part paragraphs=true/>
          </#list>
        </@contentGroup>
        </div>
      </#if>
    </div>
    
  <#if (sectionMap?size >= 2) && entryMap?has_content>
    <div class="lib-section-links">
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
                <#--<strong>Definitions:</strong><br/>-->
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
    <div class="lib-entry-links">
      <h4><#if (sectionMap?size >= 2)>All </#if>Definitions:</h4>
      <p>
      <#list entryMap?keys?sort as entryName>
        <a href="#entry-${entryName}">${entryName}</a><#if entryName_has_next>, </#if>
      </#list>
      </p>
    </div>
  </#if>

  <#if libMap?has_content>
    <div class="lib-links">
      <h4>All libraries:</h4>
      <p>
      <#list libMap?keys?sort as libName>
        <#assign lib = libMap[libName]>
        <a href="${makeInterLibUrl(lib.libDocPath)}">${escapeText(libName)}</a><#if libName_has_next>, </#if>
      </#list>
      </p>
    </div>
  </#if>
  
  </div>
  
    <#-- NOTE: there is a global entryMap, and each section also has its own entryMap -->

  <#list sectionMap?keys as sectionName> 
    <#assign section = sectionMap[sectionName]>
    <#-- print "none" below instead
    <#if section.entryMap?has_content>-->

    <#if section.title?has_content>
      <hr />  
    </#if>

    <#if section.entryMap?has_content || sectionName != "default">
      <a name="section-${sectionName}"></a>
      <div class="lib-section">
        <#if section.title?has_content> <#-- NOTE: default section has no title -->
          <h2 class="lib-section-title"><@labelText text=section.title /></h2>
        </#if>

        <#if section.comment?has_content>
          <p><@complexContent text=section.comment /></p>
        </#if>

      <#if section.entryMap?has_content>
        <#list section.entryMap?keys as entryName>
          <#assign entry = section.entryMap[entryName]>
          <#if entry.isImplemented>
          
          <hr />
          <a name="entry-${entryName}"></a>
          <div class="lib-entry lib-entry-type-${entry.type}">
            <#assign entryTitle = entry.title!"">
            <#if !entryTitle?has_content>
              <#assign entryTitle = entryName>
            </#if>

            <#-- NOTE: title is sometimes same as formal name (below), but not always -->
            <h3 class="lib-entry-title"><@labelText text=entryTitle /></h3>
            
            <div class="lib-entry-formal">
               <#-- type is "macro", "function" or "variable" -->
               <h4><span class="lib-entry-type-text">${entry.type}</span> <span class="lib-entry-formalname"><code>${entryName}</code></span><#if entry.isAbstract> (abstract/placeholder)</#if>
                  <#if entry.isDeprecated> <strong>(DEPRECATED)</strong></#if><#if entry.isOverride> <strong>(override)</strong></#if></h4>
            </div>
            
            <#global parametersSectionRendered = false>
            
            <#-- Needs special handling -->
            <#macro parametersSection entry entrySection={}>
                <#if !parametersSectionRendered>
                  <div class="lib-entry-section-params">
                    <h4><@labelText text=entrySection.title!"Parameters" /></h4>
                    
                  <#if false>
                    <div class="lib-entry-params-formal"><#--<em>All parameters:</em>-->
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
                      <div class="lib-entry-params-doc">
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
                            <div class="lib-entry-paramsection lib-entry-paramsection-${paramSectionName}">
                              <h5><@labelText text=paramSection.title /></h5>
                              <@parametersTable paramDescMaps=paramSection.paramDescMap exclude=exclude defaultDesc=defaultDesc />
                            </div>
                          </#list>
                          <#if entry.argMapUnaccounted?has_content>
                            <div class="lib-entry-paramsection lib-entry-paramsection-unlisted">
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
                      <div class="lib-entry-params-details">
                        <p>
                      <#if entry.type == "function">
                        <strong>NOTE:</strong> This function implements an advanced arguments interface emulating named
                            parameters using maps, and the parameters above may not be positional. See <a href="${makeInterLibUrl("standard/htmlTemplate")}">standard/htmlTemplate</a> for details.
                      <#elseif entry.type == "macro">
                        <strong>NOTE:</strong> This macro implements an advanced arguments interface supplementing
                            regular macro invocations. See <a href="${makeInterLibUrl("standard/htmlTemplate")}">standard/htmlTemplate</a> for details.
                      </#if>
                        </p>
                      </div>
                    </#if>
                  </div>
                </#if>
                <#global parametersSectionRendered = true>
            </#macro>

            <#macro parametersTable paramDescMaps exclude=[] defaultDesc={}>
              <table class="entry-parameters">
                <#if !paramDescMaps?is_sequence>
                  <#local paramDescMaps = [paramDescMaps]>
                </#if>
                <#list paramDescMaps as paramDescMap>
                  <#if paramDescMap?has_content>
                  <#list paramDescMap?keys as paramName>
                    <#if !exclude?seq_contains(paramName)>
                      <#assign paramDesc = paramDescMap[paramName]!"">
                      <tr>
                        <td class="entry-paramname">
                            <code>${escapeText(paramName)}</code>
                        </td>
                        <td class="entry-paramdesc">
                          <#-- TODO? the type str can be highlighted by is currently not parsed -->
                        <@contentGroup>
                        <#if paramDesc?has_content || defaultDesc?has_content>
                          <#if !paramDesc?has_content>
                            <#local paramDesc = defaultDesc>
                          </#if>
                          <#if paramDesc.typeStr?has_content>
                            <span class="lib-entry-param-desc-typeinfo"><code>${escapeText(paramDesc.typeStr!"")}</code></span>
                          </#if>
                          <#if paramDesc.shortDesc?has_content>
                            <span class="lib-entry-param-desc-shortdesc"><@descText text=paramDesc.shortDesc!"" /></span>
                          </#if>
                          <#if paramDesc.extraDesc?has_content>
                            <br/>
                            <span class="lib-entry-param-desc-extradesc"><@complexContent text=paramDesc.extraDesc!"" paragraphs=false/></span>
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
                  <div class="lib-entry-section-maindesc">
                  <@contentGroup>
                    <#if (entrySection.shortDesc)?has_content>
                      <p class="lib-entry-shortdesc"><@descText text=entrySection.shortDesc /></p>
                    </#if>
                    <#if (entrySection.extraDesc)?has_content>
                      <div class="lib-entry-extradesc">
                        <#list tmplHelper.splitToParagraphs(entrySection.extraDesc) as part>
                          <@complexContent text=part paragraphs=true />
                        </#list>
                      </div>
                    </#if>
                  </@contentGroup>
                  </div>
                <#elseif entrySectionName == "examples">
                  <div class="lib-entry-section-examples">
                    <h4><@labelText text=entrySection.title!"" /></h4>
                    <@preformedText text=entrySection.rawText!"" />
                  </div>
                <#elseif entrySectionName == "parameters">
                  <@parametersSection entry=entry entrySection=entrySection />
                <#elseif entrySectionName == "returnValues">
                  <@parametersSection entry=entry entrySection=entrySections.parameters!{} /><#-- Guarantee parameters before return values -->
                  <div class="lib-entry-section-return">
                    <h4><@labelText text=entrySection.title!"" /></h4>
                    <p>
                      <@descText text=entrySection.text!"" />
                    </p>
                  </div>
                <#elseif entrySectionName == "related">
                  <@parametersSection entry=entry entrySection=entrySections.parameters!{} /><#-- Guarantee parameters before related -->
                  <div class="lib-entry-section-related">
                     <h4><@labelText text=entrySection.title!"" /></h4>
                     <p>
                     <#list entry.relatedNames![] as name>
                       <@entryRef name=name /><#if name_has_next>, </#if>
                     </#list>
                     </p>
                  </div>  
                <#else>
                  <#--<@parametersSection entry=entry entrySection=entrySections.parameters!{} /> Don't force parameters before others -->
                  <div class="lib-entry-section-${entrySection.name} lib-entry-section-other">
                    <h4><@labelText text=entrySection.title!"" /></h4>
                    <@complexContent text=entrySection.text!"" paragraphs=true />
                  </div>
                </#if>     
            
            </#list>
            <@parametersSection entrySection=entrySections.parameters!{} entry=entry />

            <#if entry.isTransform || (entry.isAdvancedArgs!false)>
              <div class="lib-entry-details">
                <p>
                  <#if entry.isTransform><span class="lib-entry-detail">Implemented as transform.</span></#if>
                </p>
              </div>
            </#if>
            
          </div>
          
          </#if>
        </#list>
      <#else>
          <p><em>(No public definitions in this section)</em></p>
      </#if>
      </div>
    </#if>
  </#list>

</div>

<#else>
  <#if libFormat??>
     <@alert type="error">ERROR: This doc template can only render scipio-lib format FTL lib docs.</@alert>
  <#else>
     <#-- data model missing, already flagged by groovy -->
  </#if>
</#if>


