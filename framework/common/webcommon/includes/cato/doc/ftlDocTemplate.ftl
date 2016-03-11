<#--
* Cato FTL doc formatting template
*
* Invoke by running
*   ant docs-ftl-cato
* in root project folder.
*
* See FtlDocCompiler class for data model details.
*
* WARN: NO Ofbiz libraries are available here; treat this as separate project.
*
* WARN: Input documentation formatting (parsing) is sensitive to whitespace, presence and number of asterisks (*),
*    and line endings. Must be spaces-only and LF-only line endings.
*
-->
<!DOCTYPE html>
<html lang="en-US">
<#if (libFormat!"") == "cato-lib">
  <head>
    <meta content="text/html; charset=UTF-8" http-equiv="Content-Type" />
    <#if pageTitle?has_content>
      <title>${pageTitle?html}</title>
    </#if>

<#-- 
*************************************
* STYLE *
*************************************
-->

<style type="text/css">
table.entry-parameters {
  width: 100%;
  border: 1px outset black;
}

table.entry-parameters td {
  padding-left: 1em;
  padding-right: 1em;
  padding-top: 0.5em;
  padding-bottom: 0.5em;
  border: 1px solid black;
}

table.entry-parameters td.entry-paramname {
  width: 20%;
}

.lib-entry-detail {
  font-style: italic;
  font-size: 0.6em;
}

.lib-entry-formalname {
  font-weight: bold;
}

.lib-entry-param-desc-extradesc {
  font-size: 0.8em;
}

.lib-entry-params-details {
  font-size: 0.8em;
}
</style>

  </head>
  <body>
  
  
<#-- 
*************************************
* LINK FUNCTIONS *
*************************************
-->

<#function makeInterLibUrl targetLibDocPath targetName="">
  <#if !targetLibDocPath?ends_with(".html")>
    <#local targetLibDocPath = targetLibDocPath + ".html">
  </#if>
  <#local relLibDocPath = tmplHelper.getTargetRelLibDocPath(targetLibDocPath, libDocPath)!""><#t>
  <#if targetName?has_content>
    <#return relLibDocPath + "#" + targetName>
  <#else>
    <#return relLibDocPath>
  </#if>
</#function>
  
<#-- reference to another entry. name is either a full reference or name only, with or without @ or #. -->
<#macro entryRef name>
  <#local searchRes = tmplHelper.findEntryGlobal(name, entryMap, libMap)!false>
  <#if !searchRes?is_boolean>
    <#if searchRes.libDocPath?has_content>
      <a href="${makeInterLibUrl(searchRes.libDocPath, "entry-" + searchRes.rawName)}">${name?html}</a>
    <#else>
      <a href="#entry-${searchRes.rawName}">${name?html}</a>
    </#if>
  </#if>
</#macro>


<#-- 
*************************************
* TEXT PARSING AND DECORATION *
*************************************
-->

<#function createAutoLinks text>
  <#local res = "">
  <#list tmplHelper.splitByLibEntryRefs(text, entryMap, libMap) as entry>
    <#if entry?is_hash>
      <#-- got a match -->
      <#local linkMarkup>
        <#if entry.libDocPath?has_content>
          <a href="${makeInterLibUrl(entry.libDocPath, "entry-" + entry.rawName)}">${entry.name?html}</a><#t>
        <#else>
          <a href="#entry-${entry.rawName}">${entry.name?html}</a><#t>
        </#if>
      </#local>
      <#local res = res + linkMarkup>
    <#else>
      <#-- it's just text, OR an entry that didn't resolve -->
      <#local res = res + entry>
    </#if>
  </#list>
  <#return res>
</#function>

<#-- NOTE: some of these should be stripped completely, can't do it from here -->
<#assign keyIntroWords = ["WARN", "WARNING", "TODO", "FIXME", 
  "DEV NOTES?", "IMPL NOTES?", "IMPLEMENTATION NOTES?",
  <#-- this matches "NOTE" alone, but exclude the ones above -->
 "((?<!(DEV|IMPL|IMPLEMENTATION) )NOTES?)", 
 "IMPORTANT"]>
<#assign keyIntroWordsStr = keyIntroWords?join("|")>

<#function highlightWords text>
  <#return text?replace("(("+keyIntroWordsStr+")([?]:|[?]|:))", "<strong>$1</strong>", 'r')>
</#function>

<#-- creates line breaks before key into words if they aren't at beginning of text. 
    hackish but should work out okay without having massive parsing attack.
    Have to do this in two calls due to overlapping patterns
     (?<!^) -->
<#function parseIntroWords text>
  <#return text?replace("(?<!^)(("+keyIntroWordsStr+")([?]:|[?]|:))", "<br/>$1", 'r')>
</#function>

<#-- high-level version of above -->
<#function decorateText text>
  <#-- NOTE: always parse before highlight -->
  <#return createAutoLinks(highlightWords(parseIntroWords(text)))>
</#function>

<#-- 
*************************************
* BASIC TEXT WRAPPERS *
*************************************
-->


<#-- NOTE: don't decorateText this because usually contains examples and markup -->
<#macro preformedText text>
  <pre>${highlightWords(text?html)}</pre><#t><#-- ?replace("\n", "<br/>") -->
</#macro>

<#-- For titles and short labels -->
<#macro labelText text>
  ${text?html}<#t>
</#macro>

<#-- For longer descriptions -->
<#macro descText text>
  ${decorateText(text?html)}<#t>
</#macro>

<#-- 
*************************************
* HIGH LEVEL ELEM WRAPPERS *
*************************************
-->
  
<#macro descList listInfo topLevel=true>
  <#if listInfo.leadingText?has_content>
    <@descText text=listInfo.leadingText />
  </#if>
  <ul>
    <#list listInfo.items as item>
      <li>
        <#if item.items?has_content>
          <@descList listInfo=item topLevel=false/>
        <#else>
          <@descText text=item.leadingText!"" />
        </#if>
      </li>
    </#list>
  </ul>
</#macro>  

<#macro descContent text>
  <#local dataInfo = tmplHelper.parseDesc(text)>
  <#if dataInfo.type == "title">
    <h4><@labelText text=dataInfo.value!"" /></h4><#t>
  <#elseif dataInfo.type == "list">
    <div><@descList listInfo=dataInfo.value /></div>
  <#else>
    <p><@descText text=dataInfo.value!"" /></p><#t>
  </#if>
</#macro>


<#function parseDescContentNew text>
  <#local res = "">
  <#list tmplHelper.splitByLists(text) as entry>
    <#if entry?is_string>
      <#local textMarkup><@descText text=entry /></#local>
      <#local res = res + textMarkup>
    <#elseif (entry.type!"") == "list">
      <#local listMarkup><@descList listInfo=entry /></#local>
      <#local res = res + listMarkup>
    <#else>
      <#-- shouldn't happen... -->
    </#if>
  </#list>
  <#return res>
</#function>

<#-- will have better list parsing... 
   TODO: missing title parsing -->
<#macro descContentNew text>
  ${parseDescContentNew(text)}<#t>
</#macro>

  
<#-- 
*************************************
* MAIN PAGE *
*************************************
-->  
  
    <div>
      <#if pageTitle?has_content>
        <h1><@labelText text=pageTitle /></h1>
      </#if>
      <#if libFilename?has_content>
        <p><em>${libFilename?html}</em></p>
      </#if>

      <#if pageDesc?has_content>
        <#list tmplHelper.splitToParagraphs(pageDesc) as paragraph>
          <@descContent text=paragraph />
        </#list>
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
                </a><br/>
                <#--<strong>Definitions:</strong><br/>-->
                <p>
                <#list section.entryMap?keys?sort as entryName>
                  <a href="#entry-${entryName?html}">${entryName?html}</a><#if entryName_has_next>, </#if>
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
        <a href="#entry-${entryName?html}">${entryName?html}</a><#if entryName_has_next>, </#if>
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
        <a href="${makeInterLibUrl(lib.libDocPath)}">${libName?html}</a><#if libName_has_next>, </#if>
      </#list>
      </p>
    </div>
  </#if>
    
    <#-- NOTE: there is a global entryMap, and each section also has its own entryMap -->

  <#list sectionMap?keys as sectionName> 
    <#assign section = sectionMap[sectionName]>
    <#if section.entryMap?has_content>

    <#if section.title?has_content>
      <hr />  
    </#if>

      <a name="section-${sectionName?html}"></a>
      <div class="lib-section">
        <#if section.title?has_content> <#-- NOTE: default section has no title -->
          <h2 class="lib-section-title"><@labelText text=section.title /></h2>
        </#if>
    
        <#list section.entryMap?keys as entryName>
          <#assign entry = section.entryMap[entryName]>
          <#if entry.isImplemented>
          
          <hr />
          <a name="entry-${entryName?html}"></a>
          <div class="lib-entry lib-entry-type-${entry.type}">
            <#assign entryTitle = entry.title!"">
            <#if !entryTitle?has_content>
              <#assign entryTitle = entryName>
            </#if>

            <#-- NOTE: title is sometimes same as formal name (below), but not always -->
            <h3 class="lib-entry-title"><@labelText text=entryTitle /></h3>
            
            <div class="lib-entry-formal">
               <#-- type is "macro", "function" or "variable" -->
               <h4><span class="lib-entry-type-text">${entry.type}</span> <span class="lib-entry-formalname"><code>${entryName?html}</code></span><#if entry.isAbstract> (abstract/placeholder)</#if></h4>
                  <#if entry.isDeprecated> <strong>(DEPRECATED)</strong></#if><#if entry.isOverride> <strong>(override)</strong></#if>  
            </div>
            
            <#global parametersSectionRendered = false>
            
            <#macro parametersTable paramDescMap>
              <table class="entry-parameters">
                <#list paramDescMap?keys as paramName>
                  <#assign paramDesc = paramDescMap[paramName]!"">
                  <tr>
                    <td class="entry-paramname">
                        <code>${paramName?html}</code>
                    </td>
                    <td class="entry-paramdesc">
                      <#-- TODO? the type str can be highlighted by is currently not parsed -->
                      <span class="lib-entry-param-desc-typeinfo"><code>${(paramDesc.typeStr!"")?html}</code></span>
                      <span class="lib-entry-param-desc-shortdesc"><@descText text=paramDesc.shortDesc!"" /></span><br/>
                      <span class="lib-entry-param-desc-extradesc"><@descText text=paramDesc.extraDesc!"" /></span>
                    </td>
                  </tr>
                </#list>
              </table>
            </#macro>
            
            <#-- Needs special handling -->
            <#macro parametersSection entrySection entry>
                <#if !parametersSectionRendered>
                  <div class="lib-entry-section-params">
                    <h4><@labelText text=entrySection.title!"Parameters" /></h4>
                    
                    <div class="lib-entry-params-formal"><#--<em>All parameters:</em>-->
                      <p>
                      <#-- NOTE: do not sort -->
                      <#if entry.argList?has_content>
                        <#list entry.argList as argName>
                          <code>${argName?html}</code><#if argName_has_next>, </#if>
                        </#list>
                      <#else>
                        (none)
                      </#if>
                      </p>
                    </div>
                    <#-- NOTE: there is an entry-wide paramDescMap, and each param section has one too -->
                    <#if entrySection.paramDescMap?has_content>
                      <div class="lib-entry-params-doc">
                        <#if (entrySection.paramSectionMap?size >= 2)>
                          <#list entrySection.paramSectionMap?keys as paramSectionName>
                            <#assign paramSection = entrySection.paramSectionMap[paramSectionName]>
                            <div class="lib-entry-paramsection lib-entry-paramsection-${paramSectionName?html}">
                              <h5><@labelText text=paramSection.title /></h5>
                              <@parametersTable paramDescMap=paramSection.paramDescMap />
                            </div>
                          </#list>
                        <#else>
                          <@parametersTable paramDescMap=entry.paramDescMap />
                        </#if>
                      </div>
                    </#if>

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

            <#assign entrySections = entry.sections>
            <#list entrySections?keys as entrySectionName>
              <#assign entrySection = entrySections[entrySectionName]>
              
                <#if entrySectionName == "mainDesc">
                  <div class="lib-entry-section-maindesc">
                    <#if (entrySection.shortDesc)?has_content>
                      <p class="lib-entry-shortdesc">${entrySections.mainDesc.shortDesc?html}</p>
                    </#if>
                    <#if (entrySection.extraDesc)?has_content>
                      <#list tmplHelper.splitToParagraphs(entrySection.extraDesc) as paragraph>
                        <@descContent text=paragraph />
                      </#list>
                    </#if>
                  </div>
                <#elseif entrySectionName == "examples">
                  <div class="lib-entry-section-examples">
                    <h4><@labelText text=entrySection.title!"" /></h4>
                    <@preformedText text=entrySection.rawText!"" />
                  </div>
                <#elseif entrySectionName == "parameters">
                  <@parametersSection entrySection=entrySection entry=entry />
                <#elseif entrySectionName == "returnValues">
                  <@parametersSection entrySection={} entry=entry />
                  <div class="lib-entry-section-return">
                    <h4><@labelText text=entrySection.title!"" /></h4>
                    <p>
                      <@descText text=entrySection.text!"" />
                    </p>
                  </div>
                <#elseif entrySectionName == "related">
                  <@parametersSection entrySection={} entry=entry />
                  <div class="lib-entry-section-related">
                     <h4><@labelText text=entrySection.title!"" /></h4>
                     <p>
                     <#list entry.relatedNames![] as name>
                       <@entryRef name=name /><#if name_has_next>, </#if>
                     </#list>
                     </p>
                  </div>  
                <#else>
                  <@parametersSection entrySection={} entry=entry />
                  <div class="lib-entry-section-${entrySection.name?html} lib-entry-section-other">
                    <h4><@labelText text=entrySection.title!"" /></h4>
                    <@descText text=entrySection.text!"" />
                  </div>
                </#if>     
            
            </#list>
            <@parametersSection entrySection={} entry=entry />

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
      </div>
      
    </#if>
  </#list>

    <footer id="footer">
      <div>
        <hr />
        <small>
          Copyright (c) 2014-${.now?string("yyyy")} <a href="http://www.ilscipio.com" target="_blank">ilscipio GmbH</a>. Powered by <a href="http://www.cato-commerce.com" target="_blank">Cato Commerce</a> &amp; <a href="http://ofbiz.apache.org/" target=_blank>OFBiz</a>.
        </small>
      </div>
    </footer>

  </body>
<#else>
  <head><title>ERROR</title></head>
  <body><p>ERROR: This doc template can only render cato-lib format FTL lib docs.</p></body>
</#if>
</html>