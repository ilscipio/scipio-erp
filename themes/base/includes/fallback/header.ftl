<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<#include "../common.ftl">
<#-- Applications -->
<#if (requestAttributes.externalLoginKey)??><#assign externalKeyParam = "?externalLoginKey=" + (requestAttributes.externalLoginKey!)></#if>
<#if (externalLoginKey)??><#assign externalKeyParam = "?externalLoginKey=" + (requestAttributes.externalLoginKey!)></#if>
<#assign ofbizServerName = application.getAttribute("_serverId")!"default-server">
<#assign contextPath = request.getContextPath()>

<#macro generalMenu>
</#macro>

  <@scripts output=true> <#-- ensure @script elems here will always output -->

  <title>${layoutSettings.companyName}<#if title?has_content>: ${title}<#elseif titleProperty?has_content>: ${uiLabelMap[titleProperty]}</#if></title>
    
    <#if layoutSettings.shortcutIcon?has_content>
      <#assign shortcutIcon = layoutSettings.shortcutIcon/>
    <#elseif layoutSettings.VT_SHORTCUT_ICON?has_content>
      <#assign shortcutIcon = layoutSettings.VT_SHORTCUT_ICON.get(0)/>
    </#if>
    <#if shortcutIcon?has_content>
      <link rel="shortcut icon" href="<@contentUrl>${raw(shortcutIcon)}</@contentUrl>" />
    </#if>
    
    <#if layoutSettings.styleSheets?has_content>
        <#--layoutSettings.styleSheets is a list of style sheets. So, you can have a user-specified "main" style sheet, AND a component style sheet.-->
        <#list layoutSettings.styleSheets as styleSheet>
            <link rel="stylesheet" href="<@contentUrl>${raw(styleSheet)}</@contentUrl>" type="text/css"/>
        </#list>
    </#if>
    <#if layoutSettings.VT_STYLESHEET?has_content>
        <#list layoutSettings.VT_STYLESHEET as styleSheet>
            <link rel="stylesheet" href="<@contentUrl>${raw(styleSheet)}</@contentUrl>" type="text/css"/>
        </#list>
    </#if>
    <#if layoutSettings.rtlStyleSheets?has_content && langDir == "rtl">
        <#--layoutSettings.rtlStyleSheets is a list of rtl style sheets.-->
        <#list layoutSettings.rtlStyleSheets as styleSheet>
            <link rel="stylesheet" href="<@contentUrl>${raw(styleSheet)}</@contentUrl>" type="text/css"/>
        </#list>
    </#if>
    <#if layoutSettings.VT_RTL_STYLESHEET?has_content && langDir == "rtl">
        <#list layoutSettings.VT_RTL_STYLESHEET as styleSheet>
            <link rel="stylesheet" href="<@contentUrl>${raw(styleSheet)}</@contentUrl>" type="text/css"/>
        </#list>
    </#if>
    
    <#-- VT_TOP_JAVASCRIPT must always come before all others and at top of document -->
    <#if layoutSettings.VT_TOP_JAVASCRIPT?has_content>
        <#assign javaScriptsSet = toSet(layoutSettings.VT_TOP_JAVASCRIPT)/>
        <#list layoutSettings.VT_TOP_JAVASCRIPT as javaScript>
            <#if javaScriptsSet.contains(javaScript)>
                <#assign nothing = javaScriptsSet.remove(javaScript)/>
                <@script src=makeContentUrl(javaScript) />
            </#if>
        </#list>
    </#if>

    <#-- VT_PRIO_JAVASCRIPT should come right before javaScripts (always move together with javaScripts) -->
    <#if layoutSettings.VT_PRIO_JAVASCRIPT?has_content>
        <#assign javaScriptsSet = toSet(layoutSettings.VT_PRIO_JAVASCRIPT)/>
        <#list layoutSettings.VT_PRIO_JAVASCRIPT as javaScript>
            <#if javaScriptsSet.contains(javaScript)>
                <#assign nothing = javaScriptsSet.remove(javaScript)/>
                <@script src=makeContentUrl(javaScript) />
            </#if>
        </#list>
    </#if>
    <#if layoutSettings.javaScripts?has_content>
        <#--layoutSettings.javaScripts is a list of java scripts. -->
        <#-- use a Set to make sure each javascript is declared only once, but iterate the list to maintain the correct order -->
        <#assign javaScriptsSet = toSet(layoutSettings.javaScripts)/>
        <#list layoutSettings.javaScripts as javaScript>
            <#if javaScriptsSet.contains(javaScript)>
                <#assign nothing = javaScriptsSet.remove(javaScript)/>
                <@script src=makeContentUrl(javaScript) />
            </#if>
        </#list>
    </#if>
    <#if layoutSettings.VT_HDR_JAVASCRIPT?has_content>
        <#assign javaScriptsSet = toSet(layoutSettings.VT_HDR_JAVASCRIPT)/>
        <#list layoutSettings.VT_HDR_JAVASCRIPT as javaScript>
            <#if javaScriptsSet.contains(javaScript)>
                <#assign nothing = javaScriptsSet.remove(javaScript)/>
                <@script src=makeContentUrl(javaScript) />
            </#if>
        </#list>
    </#if>
    <#if layoutSettings.VT_EXTRA_HEAD?has_content>
        <#list layoutSettings.VT_EXTRA_HEAD as extraHead>
            ${extraHead}
        </#list>
    </#if>
    <#if lastParameters??><#assign parametersURL = "&amp;" + lastParameters></#if>
    <#if layoutSettings.WEB_ANALYTICS?has_content>
      <@script>
        <#list layoutSettings.WEB_ANALYTICS as webAnalyticsConfig>
          ${raw(webAnalyticsConfig.webAnalyticsCode!)}
        </#list>
      </@script>
    </#if>

  </@scripts>
</head>
<#if layoutSettings.headerImageLinkUrl??>
  <#assign logoLinkURL = "${layoutSettings.headerImageLinkUrl}">
<#else>
  <#assign logoLinkURL = "${layoutSettings.commonHeaderImageLinkUrl}">
</#if>
<#assign organizationLogoLinkURL = "${layoutSettings.organizationLogoLinkUrl!}">
<body class="<#if activeApp?has_content>app-${activeApp}</#if><#if parameters._CURRENT_VIEW_?has_content> page-${parameters._CURRENT_VIEW_!}</#if> <#if userLogin??>page-auth<#else>page-noauth</#if>">
<div class="off-canvas-wrap" data-offcanvas id="body-content">
<div class="inner-wrap">
    <!-- Off Canvas Menu -->
    <aside class="right-off-canvas-menu">
        <!-- whatever you want goes here -->
        <ul class="off-canvas-list">
          <@generalMenu />
          <#-- <#assign portalPageParamStr><#if parameters.portalPageId?has_content>&portalPageId=${raw(parameters.portalPageId!)}</#if></#assign>
          <li class="has-form"><@modal label=uiLabelMap.CommonHelp id="help" href=makePageUrl("showHelp?helpTopic=${raw(helpTopic!)}${portalPageParamStr}")></@modal></li>
          -->
        </ul>
    </aside>
    <nav class="tab-bar show-for-small">
        <section class="left-small">
            <a class="left-off-canvas-toggle menu-icon"><span></span></a>
        </section>
        <section class="middle tab-bar-section">
           
        </section>
        <section class="right-small">
            <a class="right-off-canvas-toggle menu-icon"><span></span></a>
        </section>
    </nav>
    
    <#-- 
    <nav class="top-bar hide-for-small" data-topbar role="navigation">
      <ul class="title-area">
        <li class="name">
            
          </li>
      </ul>
    
      <section class="top-bar-section">
         Right Nav Section 
        <ul class="right">          
          <li class="divider"></li>
        </ul>-->
