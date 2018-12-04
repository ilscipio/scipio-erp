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

<#macro notificationsMenu>
	<ul class="dropdown">
	    <li class="not-click"><label>${uiLabelMap["CommonLastSytemNotes"]}</label></li>
	    <#list systemNotifications as notification>
	        <li>
	            <#if notification.url?has_content><#assign notificationUrl=addParamsToUrl(notification.url,{"scipioSysMsgId":notification.messageId})></#if>
	            <a href="${notificationUrl!"#"}">
	                <div class="message_wrap <#if notification.isRead?has_content && notification.isRead=='Y'>message_isread</#if>">
	                    <#--<div class="message_status">
	                        <#if notification.fromPartyId?has_content> <span class="message_user"><small>${notification.fromPartyId!""}</small></span></#if>
	                    </div>-->
	                    <div class="message_header">
	                        ${notification.title!"No Title"} <span class="message_time right">${notification.createdStamp?string.short}</span>
	                    </div>
	                    <div class="message_body">${notification.description!""}</div>
	                </div>
	            </a>
	        </li>
	    </#list>
	</ul>
</#macro>

<#macro logoMenu hasLink=true isSmall=false>
    <#if layoutSettings.headerImageUrl??>
        <#assign headerImageUrl = layoutSettings.headerImageUrl>
    <#elseif layoutSettings.commonHeaderImageUrl??>
        <#assign headerImageUrl = layoutSettings.commonHeaderImageUrl>
    <#elseif layoutSettings.VT_HDR_IMAGE_URL??>
        <#assign headerImageUrl = layoutSettings.VT_HDR_IMAGE_URL.get(0)>
    </#if>
    <#if headerImageUrl??>
        <#if organizationLogoLinkURL?has_content>
            <#if hasLink><a href="#"></#if><img alt="${layoutSettings.companyName}" src="<@ofbizContentUrl>${rawString(organizationLogoLinkURL)}</@ofbizContentUrl>"/><#if hasLink></a></#if>
            <#else><#if hasLink><a href="#"></#if><img alt="${layoutSettings.companyName}" src="<@ofbizContentUrl>${rawString(headerImageUrl)}</@ofbizContentUrl>"/><#if hasLink></a></#if>
        </#if>
        <#else>
        <a href="#"><img alt="${layoutSettings.companyName}" src="<@ofbizContentUrl>/images/scipio/<#if isSmall>scipio-logo-small.png<#else>scipio-logo.svg</#if></@ofbizContentUrl>"/></a>
    </#if>
</#macro>

  <@scripts output=true> <#-- ensure @script elems here will always output -->

  <title>${layoutSettings.companyName}<#if title?has_content>: ${title}<#elseif titleProperty?has_content>: ${uiLabelMap[titleProperty]}</#if></title>
    
    <#if layoutSettings.shortcutIcon?has_content>
      <#assign shortcutIcon = layoutSettings.shortcutIcon/>
    <#elseif layoutSettings.VT_SHORTCUT_ICON?has_content>
      <#assign shortcutIcon = layoutSettings.VT_SHORTCUT_ICON.get(0)/>
    </#if>
    <#if shortcutIcon?has_content>
      <link rel="shortcut icon" href="<@ofbizContentUrl>${rawString(shortcutIcon)}</@ofbizContentUrl>" />
    </#if>
    
    <#if layoutSettings.styleSheets?has_content>
        <#--layoutSettings.styleSheets is a list of style sheets. So, you can have a user-specified "main" style sheet, AND a component style sheet.-->
        <#list layoutSettings.styleSheets as styleSheet>
            <link rel="stylesheet" href="<@ofbizContentUrl>${rawString(styleSheet)}</@ofbizContentUrl>" type="text/css"/>
        </#list>
    </#if>
    <#if layoutSettings.VT_STYLESHEET?has_content>
        <#list layoutSettings.VT_STYLESHEET as styleSheet>
            <link rel="stylesheet" href="<@ofbizContentUrl>${rawString(styleSheet)}</@ofbizContentUrl>" type="text/css"/>
        </#list>
    </#if>
    <#if layoutSettings.rtlStyleSheets?has_content && langDir == "rtl">
        <#--layoutSettings.rtlStyleSheets is a list of rtl style sheets.-->
        <#list layoutSettings.rtlStyleSheets as styleSheet>
            <link rel="stylesheet" href="<@ofbizContentUrl>${rawString(styleSheet)}</@ofbizContentUrl>" type="text/css"/>
        </#list>
    </#if>
    <#if layoutSettings.VT_RTL_STYLESHEET?has_content && langDir == "rtl">
        <#list layoutSettings.VT_RTL_STYLESHEET as styleSheet>
            <link rel="stylesheet" href="<@ofbizContentUrl>${rawString(styleSheet)}</@ofbizContentUrl>" type="text/css"/>
        </#list>
    </#if>
    
    <#-- VT_TOP_JAVASCRIPT must always come before all others and at top of document -->
    <#if layoutSettings.VT_TOP_JAVASCRIPT?has_content>
        <#assign javaScriptsSet = toSet(layoutSettings.VT_TOP_JAVASCRIPT)/>
        <#list layoutSettings.VT_TOP_JAVASCRIPT as javaScript>
            <#if javaScriptsSet.contains(javaScript)>
                <#assign nothing = javaScriptsSet.remove(javaScript)/>
                <@script src=makeOfbizContentUrl(javaScript) />
            </#if>
        </#list>
    </#if>

    <#-- VT_PRIO_JAVASCRIPT should come right before javaScripts (always move together with javaScripts) -->
    <#if layoutSettings.VT_PRIO_JAVASCRIPT?has_content>
        <#assign javaScriptsSet = toSet(layoutSettings.VT_PRIO_JAVASCRIPT)/>
        <#list layoutSettings.VT_PRIO_JAVASCRIPT as javaScript>
            <#if javaScriptsSet.contains(javaScript)>
                <#assign nothing = javaScriptsSet.remove(javaScript)/>
                <@script src=makeOfbizContentUrl(javaScript) />
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
                <@script src=makeOfbizContentUrl(javaScript) />
            </#if>
        </#list>
    </#if>
    <#if layoutSettings.VT_HDR_JAVASCRIPT?has_content>
        <#assign javaScriptsSet = toSet(layoutSettings.VT_HDR_JAVASCRIPT)/>
        <#list layoutSettings.VT_HDR_JAVASCRIPT as javaScript>
            <#if javaScriptsSet.contains(javaScript)>
                <#assign nothing = javaScriptsSet.remove(javaScript)/>
                <@script src=makeOfbizContentUrl(javaScript) />
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
          ${rawString(webAnalyticsConfig.webAnalyticsCode!)}
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
          <#-- <#assign portalPageParamStr><#if parameters.portalPageId?has_content>&portalPageId=${rawString(parameters.portalPageId!)}</#if></#assign>
          <li class="has-form"><@modal label=uiLabelMap.CommonHelp id="help" href=makeOfbizUrl("showHelp?helpTopic=${rawString(helpTopic!)}${portalPageParamStr}")></@modal></li>
          -->
        </ul>
    </aside>
    <nav class="tab-bar show-for-small">
        <section class="left-small">
            <a class="left-off-canvas-toggle menu-icon"><span></span></a>
        </section>
        <section class="middle tab-bar-section">
            <h1><@logoMenu isSmall=true/></h1>
        </section>
        <section class="right-small">
            <a class="right-off-canvas-toggle menu-icon"><span></span></a>
        </section>
    </nav>
    
    <nav class="top-bar hide-for-small" data-topbar role="navigation">
      <ul class="title-area">
        <li class="name">
            <h1><@logoMenu isSmall=true/></h1>   
          </li>
      </ul>
    
      <section class="top-bar-section">
        <!-- Right Nav Section -->
        <ul class="right">
          <#-- Notifications -->
          <#if systemNotifications?has_content>
              <li class="has-dropdown not-click"><a href="#"><i class="${styles.icon!} ${styles.icon_prefix!}bell"></i><#if systemNotificationsCount?has_content> <span class="label">${systemNotificationsCount}</span></#if></a>
                <@notificationsMenu />
              </li>
          </#if>
          <#-- UserLogin -->
          <li class="has-dropdown not-click">            
            <ul class="dropdown">       
                <@generalMenu />
            </ul>
          </li>
          <li class="divider"></li>
        </ul>
