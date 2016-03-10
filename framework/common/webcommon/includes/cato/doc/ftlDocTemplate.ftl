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
* WARN: The parsing of the input files (and data model received here) is sensitive
*     to whitespace, indentation and line endings.
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
</style>

  </head>
  <body>
  
  
<#macro descEntryList listInfo topLevel=true>
  <@descText text=listInfo.leadingText!"" />
  <ul>
    <#list listInfo.items as item>
      <li>
        <#if item?is_hash>
          <@descEntryList listInfo=item topLevel=false/>
        <#else>
          <@descText text=item />
        </#if>
      </li>
    </#list>
  </ul>
</#macro>  
  
<#-- Helper functions -->
<#macro descEntryContent text>
  <#local dataInfo = tmplHelper.parseDescEntry(text)>
  <#if dataInfo.type == "title">
    <h4><@labelText text=dataInfo.value!"" /></h4><#t>
  <#elseif dataInfo.type == "list">
    <div><@descEntryList listInfo=dataInfo.value /></div>
  <#else>
    <p><@descText text=dataInfo.value!"" /></p><#t>
  </#if>
</#macro>

<#-- NOTE: some of these should be stripped completely, can't do it from here -->
<#assign keyIntroWords = ["WARN", "WARNING", "TODO", "TODO\\?", "FIXME", "FIXME\\?", "DEV NOTE", "DEV NOTES", 
 "IMPL NOTE", "IMPL NOTES", "IMPLEMENTATION NOTES", "NOTE", "NOTES"]>
<#assign keyIntroWordsStr = keyIntroWords?join("|")>
<#function decorateText text>
  <#return text?replace("("+keyIntroWordsStr+"):", "<strong>$1:</strong>", 'r')>
</#function>

<#macro preformedText text>
  <pre>${decorateText(text?html)}</pre><#t><#-- ?replace("\n", "<br/>") -->
</#macro>

<#-- For titles and short labels -->
<#macro labelText text>
  ${decorateText(text?html)}<#t>
</#macro>

<#-- For longer descriptions -->
<#macro descText text>
  ${decorateText(text?html)}<#t>
</#macro>

<#-- reference to another entry -->
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

  
    <div>
      <#if pageTitle?has_content>
        <h1><@labelText text=pageTitle /></h1>
      </#if>
      <#if libFilename?has_content>
        <p><em>${libFilename?html}</em></p>
      </#if>

      <#if pageDesc?has_content>
        <#list tmplHelper.splitToParagraphs(pageDesc) as paragraph>
          <@descEntryContent text=paragraph />
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
                    <#if entry.paramDescMap?has_content>
                      <div class="lib-entry-params-doc">
                      <table class="entry-parameters">
                        <#list entry.paramDescMap?keys as paramName>
                          <#assign paramDesc = entry.paramDescMap[paramName]!"">
                          <tr>
                            <td class="entry-paramname"><code>${paramName?html}</code></td>
                            <td class="entry-paramdesc"><@descText text=paramDesc.text!"" /></td>
                          </tr>
                        </#list>
                      </table>
                      </div>
                    </#if>

                    <#if entry.isAdvancedArgs>
                      <div class="lib-entry-params-details">
                        <p>
                          <em>
                      <#if entry.type == "function">
                        <strong>NOTE:</strong> This function implements an advanced arguments interface emulating named
                            parameters using maps, and the parameters above may not be positional. See <a href="${makeInterLibUrl("standard/htmlTemplate")}">standard/htmlTemplate</a> for details.
                      <#elseif entry.type == "macro">
                        <strong>NOTE:</strong> This macro implements an advanced arguments interface supplementing
                            regular macro invocations. See <a href="${makeInterLibUrl("standard/htmlTemplate")}">standard/htmlTemplate</a> for details.
                      </#if>
                          </em>
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
                        <@descEntryContent text=paragraph />
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
                  <div class="lib-entry-section-other">
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