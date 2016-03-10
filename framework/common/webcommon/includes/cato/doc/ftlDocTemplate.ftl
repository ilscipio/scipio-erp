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
  width:100%;
  border:1px outset black;
}

table.entry-parameters td {
  padding:1em;
  border:1px solid black;
}

table.entry-parameters td.entry-paramname {
  width:20%;
}

.lib-entry-detail {
  font-style:italic;
  font-size:0.6em;
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
      <#local relLibDocPath = tmplHelper.getTargetRelLibDocPath(searchRes.libDocPath, libDocPath)!"">
      <a href="${relLibDocPath}#entry-${searchRes.rawName?html}">${name?html}</a>
    <#else>
      <a href="#entry-${searchRes.rawName?html}">${name?html}</a>
    </#if>
  </#if>
</#macro>

  
    <div>
      <#if pageTitle?has_content>
        <h1><@labelText text=pageTitle /></h1>
      </#if>
      <#if libFilename?has_content>
        <p><em>${libFilename}</em></p>
      </#if>
      <#if pageDesc?has_content>
        <#list tmplHelper.splitToParagraphs(pageDesc) as paragraph>
          <@descEntryContent text=paragraph />
        </#list>
      </#if>
    </div>
    
  <#if (sectionMap?size >= 2)>
    <div>
      <strong>Sections</strong>
      <ul>
        <#list sectionMap?keys as sectionName> 
          <#assign section = sectionMap[sectionName]>
          <#if section.entryMap?has_content>
            <li><a href="#section-${sectionName}"><#rt>
                <#if section.title?has_content><@labelText text=section.title /><#t>
                <#elseif sectionName == "default"><@labelText text="MAIN" /><#t>
                <#else><@labelText text=sectionName /></#if><#t>
                </a></li><#lt>
          </#if>
        </#list>
      </ul>
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
          <div class="lib-entry">
            <#assign entryTitle = entry.title!"">
            <#if !entryTitle?has_content>
              <#assign entryTitle = entryName>
            </#if>

            <h3 class="lib-entry-title"><@labelText text=entryTitle /></h3>
            
            <div class="lib-entry-formal">
              <em>${(entry.type!"")?html}</em> <strong>${entryName?html}</strong><#if entry.isDeprecated> <strong>(DEPRECATED)</strong></#if> 
            </div>
            
            <#assign entrySections = entry.sections>
            
            <#if entrySections.mainDesc??>
              <div class="lib-entry-desc">
                <#if (entrySections.mainDesc.shortDesc)?has_content>
                  <p class="lib-entry-shortdesc">${entrySections.mainDesc.shortDesc?html}</p>
                </#if>
                <#if (entrySections.mainDesc.extraDesc)?has_content>
                  <#list tmplHelper.splitToParagraphs(entrySections.mainDesc.extraDesc) as paragraph>
                    <@descEntryContent text=paragraph />
                  </#list>
                </#if>
              </div>
            </#if>
            
            <#if entrySections.examples??>
              <div class="lib-entry-examples">
                <h4><@labelText text=entrySections.examples.title!"" /></h4>
                <@preformedText text=entry.exampleText!"" />
              </div>
            </#if>
            
            <#if entrySections.parameters??>
              <div class="lib-entry-params">
                <h4><@labelText text=entrySections.parameters.title!"" /></h4>
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
                      ${argName?html}<#if argName_has_next>,</#if>
                    </#list>
                  <#else>
                    (none)
                  </#if>
                </#if>
                </p>
              </div>
            </#if>
              
            <#if entrySections.returnValues??>
              <div class="lib-entry-return">
                <h4><@labelText text=entrySections.returnValues.title!"" /></h4>
                <@descText text=entrySections.returnValues.text!"" />
              </div>
            </#if>
            
            <#if entrySections.related??>
              <div class="lib-entry-related">
                 <h4><@labelText text=entrySections.related.title!"" /></h4>
                 NOTE: doc related is broken<br/>
                 <p>
                 <#list entry.relatedNames![] as name>
                   <@entryRef name=name /><#if name_has_next>,</#if>
                 </#list>
                 </p>
              </div>       
            </#if>     
            
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