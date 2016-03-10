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

<#function highlightKeyWords text>
  <#-- TODO: less horrible version. most of these need better formatting too. -->
  <#return text
    ?replace("WARN:", "<strong>WARN:</strong>")
    ?replace("TODO:", "<strong>TODO:</strong>")
    ?replace("WARNING:", "<strong>WARNING:</strong>")
    ?replace("DEV NOTE:", "<strong>DEV NOTE:</strong>")
    ?replace("DEV NOTES:", "<strong>DEV NOTES:</strong>")
    ?replace("IMPL NOTE:", "<strong>IMPL NOTE:</strong>")
    ?replace("IMPL NOTES:", "<strong>IMPL NOTES:</strong>")
    ?replace("IMPLEMENTATION NOTES:", "<strong>IMPLEMENTATION NOTES:</strong>")
    ?replace("NOTE:", "<strong>NOTE:</strong>")
    ?replace("NOTES:", "<strong>NOTES:</strong>")
    >
</#function>

<#macro preformedText text>
  ${highlightKeyWords(text?html)?replace("\n", "<br/>")}<#t>
</#macro>

<#-- For titles and short labels -->
<#macro labelText text>
  ${highlightKeyWords(text?html)}<#t>
</#macro>

<#-- For longer descriptions -->
<#macro descText text>
  ${highlightKeyWords(text?html)}<#t>
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
          <#assign sectionInfo = sectionMap[sectionName]>
          <#if sectionInfo.entryMap?has_content>
            <li><a href="#section-${sectionName}"><#rt>
                <#if sectionInfo.title?has_content><@labelText text=sectionInfo.title /><#t>
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
    <#assign sectionInfo = sectionMap[sectionName]>
    <#if sectionInfo.entryMap?has_content>

    <#if sectionInfo.title?has_content>
      <hr />  
    </#if>

      <a name="section-${sectionName?html}"></a>
      <div class="lib-section">
        <#if sectionInfo.title?has_content> <#-- NOTE: default section has no title -->
          <h2 class="lib-section-title"><@labelText text=sectionInfo.title /></h2>
        </#if>
    
        <#list sectionInfo.entryMap?keys as entryName>
          <#assign entryInfo = sectionInfo.entryMap[entryName]>
          <#if entryInfo.isImplemented>
          
          <hr />
          <a name="entry-${entryName?html}"></a>
          <div class="lib-entry">
            <#assign entryTitle = entryInfo.title!"">
            <#if !entryTitle?has_content>
              <#assign entryTitle = entryName>
            </#if>

            <h3 class="lib-entry-title"><@labelText text=entryTitle /></h3>
            
            <div class="lib-entry-formal">
              <#if entryInfo.isDeprecated><strong>DEPRECATED.</strong><br/></#if>
              <em>${(entryInfo.type!"")?html}</em> <strong>${entryName?html}</strong>
              <#if entryInfo.isTransform><br/><em>Implemented as transform.</strong></#if>
            </div>
            
            <#assign entrySections = entryInfo.sections>
            
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
                <@preformedText text=entrySections.examples.text!"" />
              </div>
            </#if>
            
            <#if entrySections.parameters??>
              <div class="lib-entry-params">
                <h4><@labelText text=entrySections.parameters.title!"" /></h4>
                <#if entrySections.parameters.parameters?has_content>
                  <table class="entry-parameters">
                    <#list entrySections.parameters.parameters?keys as paramName>
                      <#assign paramText = entrySections.parameters.parameters[paramName]!"">
                      <tr>
                        <td class="entry-paramname">${paramName?html}</td>
                        <#-- FIXME: @preformedText is not appropriate -->
                        <td class="entry-paramdesc"><@preformedText text=paramText /></td>
                      </tr>
                    </#list>
                  </table>
                <#else>
                  <span>(no parameters)</span>
                </#if>

                <#if entryInfo.argList??>
                  <p><em>All programmed parameters:
                  <#if entryInfo.argList?has_content>
                    <#list entryInfo.argList as argName>
                      ${argName?html}<#if argName_has_next>,</#if>
                    </#list>
                  <#else>
                    (none)
                  </#if>
                  </em>
                </#if>
                </p>
              </div>
            </#if>
              
            <#if entrySections.returnValues??>
              <div class="lib-entry-return">
                <h4><@labelText text=entrySections.returnValues.title!"" /></h4>
                <@preformedText text=entrySections.returnValues.text!"" />
              </div>
            </#if>
            
            <#if entrySections.related??>
              <div class="lib-entry-related">
                 <h4><@labelText text=entrySections.related.title!"" /></h4>
                 <p>
                 <#list entrySections.related.relatedNames![] as name>
                   <@entryRef name=name /><#if name_has_next>,</#if>
                 </#list>
                 </p>
              </div>       
            </#if>     
          </div>
          
          </#if>
        </#list>
      </div>
      
      <hr />
    </#if>
  </#list>

  </body>
<#else>
  <head><title>ERROR</title></head>
  <body><p>ERROR: This doc template can only render cato-lib format FTL lib docs.</p></body>
</#if>
</html>