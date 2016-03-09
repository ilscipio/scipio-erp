<#--
* Cato FTL doc formatting template
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
  </head>
  <body>
  
  
<#macro descEntryList listInfo>
  <ul>${listInfo.title!""}
    <#list listInfo.items as item>
      <li>
        <#if item?is_hash>
          <@descEntryList listInfo=item />
        <#else>
          ${item?html}
        </#if>
      </li>
    </#list>
  </ul>
</#macro>  
  
<#-- Helper functions -->
<#macro descEntryContent text>
  <#local dataInfo = tmplHelper.parseDescEntry(text)>
  <#if dataInfo.type == "title">
    <h4>${dataInfo.value?html}</h4><#t>
  <#elseif dataInfo.type == "list">
    <@descEntryList listInfo=dataInfo.value />
  <#else>
    <p>${dataInfo.value?html}</p><#t>
  </#if>
</#macro>
  
    <div>
      <#if pageTitle?has_content>
        <h1>${pageTitle?html}</h1>
      </#if>
      <#if pageDesc?has_content>
        <#list tmplHelper.splitToParagraphs(pageDesc) as paragraph>
          <@descEntryContent text=paragraph />
        </#list>
      </#if>
    </div>

    <#-- NOTE: there is a global entryMap, and each section also has its own entryMap -->

    <#list sectionMap?keys as sectionName> 
      <#assign sectionInfo = sectionMap[sectionName]>
      <div class="lib-section">
        <#if sectionInfo.title?has_content> <#-- NOTE: default section has no title -->
          <h2 class="lib-section-title">${sectionInfo.title?html}</h2>
        </#if>
    
        <#list sectionInfo.entryMap?keys as entryName>
          <#assign entryInfo = sectionInfo.entryMap[entryName]>
          <div class="lib-entry">
            <#assign entryTitle = entryInfo.title!"">
            <#if !entryTitle?has_content>
              <#assign entryTitle = entryName>
            </#if>
            <h3 class="lib-entry-title">${entryTitle?html}</h3>
            <div class="lib-entry-formal">
             <em>${entryInfo.type!""?html}</em> <strong>${entryName?html}</strong>
            </div>
            <#if entryInfo.shortDesc?has_content>
              <p class="lib-entry-shortdesc">${entryInfo.shortDesc}</p>
            </#if>
            <#if entryInfo.extraDesc?has_content>
              <#list tmplHelper.splitToParagraphs(entryInfo.extraDesc) as paragraph>
                <@descEntryContent text=paragraph />
              </#list>
            </#if>
          </div>
        </#list>
      </div>
    </#list>

  </body>
<#else>
  <head><title>ERROR</title></head>
  <body><p>ERROR: This doc template can only render cato-lib format FTL lib docs.</p></body>
</#if>
</html>