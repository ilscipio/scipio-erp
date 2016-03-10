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
-->
<!DOCTYPE html>
<html lang="en-US">
<#if (libFormat!"") == "cato-lib">
  <head>
    <#if pageTitle?has_content>
      <title>${pageTitle?html}</title>
    </#if>

<style>
table.entry-parameters {
  width: 100%;
  border: 1px outset black;
}

table.entry-parameters td {
  padding: 1em;
  border: 1px solid black;
}

table.entry-parameters td.entry-paramname {
  width: 20%;
}

.lib-entry-detail {
  font-style: italic;
  font-size: 0.6em;
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
      <strong>Sections:</strong>
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
      <strong><#if (sectionMap?size >= 2)>All </#if>Definitions:</strong>
      <p>
      <#list entryMap?keys?sort as entryName>
        <a href="#entry-${entryName?html}">${entryName?html}</a><#if entryName_has_next>, </#if>
      </#list>
      </p>
    </div>
  </#if>

  <#if libMap?has_content>
    <div class="lib-links">
      <strong>All libraries:</strong>
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
               <em><span class="lib-entry-type-text">${entry.type}</span> <span class="lib-entry-formalname"><strong>${entryName?html}</strong></span></em>
                  <#if entry.isDeprecated> <strong>(DEPRECATED)</strong></#if><#if entry.isOverride> <strong>(override)</strong></#if>  
            </div>
            
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
                  <div class="lib-entry-section-params">
                    <h4><@labelText text=entrySection.title!"" /></h4>
                    NOTE: doc parameter parsing is broken<br/>
                    <#if entry.parameters?has_content>
                      <table class="entry-parameters">
                        <#list entry.parameters?keys as paramName>
                          <#assign paramText = entry.parameters[paramName]!"">
                          <tr>
                            <td class="entry-paramname">${paramName?html}</td>
                            <td class="entry-paramdesc"><@descText text=paramText /></td>
                          </tr>
                        </#list>
                      </table>
                    <#else>
                      <span>(no parameters)</span>
                    </#if>
    
                    <#if entry.argList??>
                      <p><em>All programmed parameters:</em>
                      <#if entry.argList?has_content>
                        <#list entry.argList as argName>
                          ${argName?html}<#if argName_has_next>, </#if>
                        </#list>
                      <#else>
                        (none)
                      </#if>
                    </#if>
                    </p>
                  </div>
                <#elseif entrySectionName == "returnValues">
                  <div class="lib-entry-section-return">
                    <h4><@labelText text=entrySection.title!"" /></h4>
                    <@descText text=entrySection.text!"" />
                  </div>
                <#elseif entrySectionName == "related">
                  <div class="lib-entry-section-related">
                     <h4><@labelText text=entrySection.title!"" /></h4>
                     <p>
                     <#list entry.relatedNames![] as name>
                       <@entryRef name=name /><#if name_has_next>, </#if>
                     </#list>
                     </p>
                  </div>  
                <#else>
                  <div class="lib-entry-section-other">
                    <h4><@labelText text=entrySection.title!"" /></h4>
                    <@descText text=entrySection.text!"" />
                  </div> 
                </#if>     
            
            </#list>
            
            <div class="lib-entry-details">
              <#if entry.isTransform><span class="lib-entry-detail">Implemented as transform.</span></#if>
              <#if entry.isAdvancedArgs!false><span class="lib-entry-detail">Supports advanced arguments interface.</span></#if>
            </div>
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