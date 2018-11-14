<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#assign docLangAttr = locale.toString()?replace("_", "-")>
<#assign langDir = "ltr">
<#if "ar.iw"?contains(docLangAttr?substring(0, 2))>
    <#assign langDir = "rtl">
</#if>
<html lang="${docLangAttr}" dir="${langDir}" xmlns="http://www.w3.org/1999/xhtml">
    <head>
        <title>${uiLabelMap.WebtoolsEntityReference}</title>
        <style type="text/css">
           body, textarea, input, select {font-family: Helvetica, sans-serif; background-color: #ffffff; text-decoration: none;}
          .section-header {font-size: 10pt; font-weight: bold; color: #000000; padding-bottom: 10;}
          .listtext {font-size: 10pt; font-weight: bold; color: blue;}
          .listtext a {text-decoration: none;}
          .listtext a:hover {color:red; text-decoration: underline;}
        </style>
    </head>
    <body>
        <div class="listtext">
            <#if !forstatic>
                <a href="<@ofbizUrl>main</@ofbizUrl>" target="main">${uiLabelMap.WebtoolsPopupWebToolsMain}</a><br/>
                <a href="<@ofbizUrl>view/entityref_main</@ofbizUrl>" target="entityFrame">${uiLabelMap.WebtoolsEntityReferenceMainPage}</a><br/>
                <a href="<@ofbizUrl>view/checkdb</@ofbizUrl>" target="entityFrame">${uiLabelMap.WebtoolsCheckUpdateDatabase}</a>
                <hr />
                <#-- want to leave these out because they are only working so-so, and cause people more problems that they solve, IMHO
                <a href="<@ofbizUrl>ModelWriter</@ofbizUrl>" target="_blank">Generate Entity Model XML (all in one)</a><br/>
                <a href="<@ofbizUrl>ModelWriter?savetofile=true</@ofbizUrl>" target="_blank">Save Entity Model XML to Files</a><br/>
                -->
                <#-- this is not working now anyway...
                <a href="<@ofbizUrl>ModelGroupWriter</@ofbizUrl>" target="_blank">Generate Entity Group XML</a><br/>
                <a href="<@ofbizUrl>ModelGroupWriter?savetofile=true</@ofbizUrl>" target="_blank">Save Entity Group XML to File</a><br/>
                -->
                <a href="<@ofbizUrl>view/ModelInduceFromDb</@ofbizUrl>" target="_blank">${uiLabelMap.WebtoolsInduceModelXMLFromDatabase}</a><br/>
            </#if>
            <#if packageNames?has_content>
                <hr />
                <div class="section-header">${uiLabelMap.WebtoolsEntityPackages}</div>
                <#list packageNames as packageName>
                    <#if forstatic>
                        <a href="<@ofbizUrl>view/entityref_main?forstatic=true#${packageName}</@ofbizUrl>" target="entityFrame">${packageName}</a><br />
                    <#else>
                        <a href="<@ofbizUrl>view/entityref_main#${packageName}</@ofbizUrl>" target="entityFrame">${packageName}</a><br />
                    </#if>
                </#list>
            </#if>
            <#if entitiesList?has_content>
                <hr />
                <div class="section-header">${uiLabelMap.WebtoolsEntitiesAlpha}</div>
                <#list entitiesList as entity>
                    <#if forstatic>
                        <a href="<@ofbizUrl>view/entityref_main?forstatic=true#${entity.entityName}</@ofbizUrl>" target="entityFrame">${entity.entityName}</a>
                    <#else>
                        <a href="<@ofbizUrl>view/entityref_main#${entity.entityName}${entity.url!}</@ofbizUrl>" target="entityFrame">${entity.entityName}</a>
                    </#if>
                    <br />
                </#list>
            </#if>
        </div>
    </body>
</html>
