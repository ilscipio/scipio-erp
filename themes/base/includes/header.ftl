<#--
Licensed to the Apache Software Foundation (ASF) under one
or more contributor license agreements.  See the NOTICE file
distributed with this work for additional information
regarding copyright ownership.  The ASF licenses this file
to you under the Apache License, Version 2.0 (the
"License"); you may not use this file except in compliance
with the License.  You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing,
software distributed under the License is distributed on an
"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
KIND, either express or implied.  See the License for the
specific language governing permissions and limitations
under the License.
-->

<#include "common.ftl">
<#-- Applications -->
<#if (requestAttributes.externalLoginKey)??><#assign externalKeyParam = "?externalLoginKey=" + (requestAttributes.externalLoginKey!)></#if>
<#if (externalLoginKey)??><#assign externalKeyParam = "?externalLoginKey=" + (requestAttributes.externalLoginKey!)></#if>
<#assign ofbizServerName = application.getAttribute("_serverId")!"default-server">
<#assign contextPath = request.getContextPath()>
<#if userLogin?has_content>
    <#assign displayApps = Static["org.ofbiz.webapp.control.LoginWorker"].getAppBarWebInfos(security, userLogin, ofbizServerName, "main")>
    <#assign displaySecondaryApps = Static["org.ofbiz.webapp.control.LoginWorker"].getAppBarWebInfos(security, userLogin, ofbizServerName, "secondary")>
    <#assign appModelMenu = Static["org.ofbiz.widget.model.MenuFactory"].getMenuFromLocation(applicationMenuLocation,applicationMenuName)>
</#if>
<#if person?has_content>
  <#assign userName = person.firstName! + " " + person.middleName! + " " + person.lastName!>
<#elseif partyGroup?has_content>
  <#assign userName = partyGroup.groupName!>
<#elseif userLogin??>
  <#assign userName = userLogin.userLoginId>
<#else>
  <#assign userName = "">
</#if>
<#if defaultOrganizationPartyGroupName?has_content>
  <#assign orgName = " - " + defaultOrganizationPartyGroupName!>
<#else>
  <#assign orgName = "">
</#if>
<#macro generalMenu>
    <#if userLogin??>
        <#--
        <#if layoutSettings.topLines?has_content>
          <#list layoutSettings.topLines as topLine>
            <#if topLine.text??>
              <li>${topLine.text}<a href="${rawString(topLine.url!)}${rawString(externalKeyParam)}">${topLine.urlText!}</a></li>
            <#elseif topLine.dropDownList??>
              <li><#include "component://common/webcommon/includes/insertDropDown.ftl"/></li>
            <#else>
              <li>${topLine!}</li>
            </#if>
          </#list>
        <#else>
          <li>${userLogin.userLoginId}</li>
        </#if>
        -->
        <li><a href="<@ofbizUrl>ListLocales</@ofbizUrl>">${uiLabelMap.CommonLanguageTitle}</a></li>
        <li><a href="<@ofbizUrl>ListVisualThemes</@ofbizUrl>">${uiLabelMap.CommonVisualThemes}</a></li>
    </#if>
    <#if parameters.componentName?? && requestAttributes._CURRENT_VIEW_?? && helpTopic??>
        <#include "component://common/webcommon/includes/helplink.ftl" />
    </#if>
    <#if userLogin??>
        <li class="divider"></li>
        <li class="active"><a href="<@ofbizUrl>logout?t=${.now?long}</@ofbizUrl>"<#-- class="alert ${styles.link_nav!}"-->>${uiLabelMap.CommonLogout}</a></li>
    </#if>
</#macro>

<#macro primaryAppsMenu>
  <#assign appCount = 0>
  <#assign firstApp = true>
    <li><label>${uiLabelMap["CommonPrimaryApps"]}</label></li>
  <#list displayApps as display>
        <#assign thisApp = display.getContextRoot()>
        <#assign selected = false>
        <#if thisApp == contextPath || contextPath + "/" == thisApp>
          <#assign selected = true>
        </#if>
        <#assign servletPath = Static["org.ofbiz.webapp.WebAppUtil"].getControlServletPathSafeSlash(display)!"">
        <#assign thisURL = rawString(servletPath)>
        <#if thisApp != "/">
          <#if servletPath?has_content>
            <#assign thisURL = thisURL + "main">
          <#else>
            <#assign thisURL = thisApp>
          </#if>
        </#if>
        <#if layoutSettings.suppressTab?? && display.name == layoutSettings.suppressTab>
          <#-- do not display this component-->
        <#else>
            <li <#if selected> class="active"</#if>>
                <a href="${thisURL}${rawString(externalKeyParam)}"<#if uiLabelMap??> title="${uiLabelMap[display.description]}">${uiLabelMap[display.title]}<#else> title="${display.description}">${display.title}</#if></a>
            </li>
            <#assign appCount = appCount + 1>
        </#if>
  </#list>
</#macro>

<#macro secondaryAppsMenu>
    <li><label>${uiLabelMap["CommonSecondaryApps"]}</label></li>
    <#list displaySecondaryApps as display>
        <#assign thisApp = display.getContextRoot()>
        <#assign selected = false>
        <#if thisApp == contextPath || contextPath + "/" == thisApp>
          <#assign selected = true>
        </#if>
          <#assign servletPath = Static["org.ofbiz.webapp.WebAppUtil"].getControlServletPathSafeSlash(display)!"">
          <#assign thisURL = rawString(servletPath)>
          <#if thisApp != "/">
            <#if servletPath?has_content>
              <#assign thisURL = thisURL + "main">
            <#else>
              <#assign thisURL = thisApp>
            </#if>
          </#if>
          <li <#if selected> class="active"</#if>>      
            <a href="${thisURL}${rawString(externalKeyParam)}"<#if selected> class="active"</#if><#if uiLabelMap??> title="${uiLabelMap[display.description]}">${uiLabelMap[display.title]}<#else> title="${display.description}">${display.title}</#if></a>
            <#assign appCount = appCount + 1>
          </li>
    </#list>
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
        <#-- 
        <li class="active">
            <a href="#">
                <div>View All</div>
            </a>
        </li>-->
</ul>
</#macro>

<#-- in theory there is a transform that converts the selected menu to a proper list on these screens. It is never used by any of the other ofbiz screens, however and poorly documented
so for now we have to split the screens in half and rely on the menu widget renderer to get the same effect
<#macro currentAppMenu>
    <#if appModelMenu?has_content>
        <li class="has-dropdown not-click active"><a href="#">${title!"TEST"}</a>
            <ul class="dropdown">
                
            </ul>
        </li>
    </#if>
</#macro>-->

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
            <#if hasLink><a href="<@ofbizUrl>${logoLinkURL}</@ofbizUrl>"></#if><img alt="${layoutSettings.companyName}" src="<@ofbizContentUrl>${rawString(organizationLogoLinkURL)}</@ofbizContentUrl>"/><#if hasLink></a></#if>
            <#else><#if hasLink><a href="<@ofbizUrl>${logoLinkURL}</@ofbizUrl>"></#if><img alt="${layoutSettings.companyName}" src="<@ofbizContentUrl>${rawString(headerImageUrl)}</@ofbizContentUrl>"/><#if hasLink></a></#if>
        </#if>
        <#else>
        <a href="<@ofbizUrl>${logoLinkURL!""}</@ofbizUrl>"><img alt="${layoutSettings.companyName}" src="<@ofbizContentUrl>/images/scipio/<#if isSmall>scipio-logo-small.png<#else>scipio-logo.svg</#if></@ofbizContentUrl>"/></a>
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
          <#assign portalPageParamStr><#if parameters.portalPageId?has_content>&portalPageId=${rawString(parameters.portalPageId!)}</#if></#assign>
          <li class="has-form"><@modal label=uiLabelMap.CommonHelp id="help" href=makeOfbizUrl("showHelp?helpTopic=${rawString(helpTopic!)}${portalPageParamStr}")></@modal></li>
        </ul>
    </aside>
    
    <aside class="left-off-canvas-menu">
      <ul class="off-canvas-list">
          <@primaryAppsMenu/>
          <@secondaryAppsMenu/>
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
            <#if userLogin??><a href="#">${uiLabelMap.CommonWelcome}! ${userLogin.userLoginId}<#else><a href="<@ofbizUrl>${checkLoginUrl}</@ofbizUrl>">${uiLabelMap.CommonLogin}</a></#if></a>
            <ul class="dropdown">       
                <@generalMenu />
            </ul>
          </li>
          <li class="divider"></li>
          <#assign portalPageParamStr><#if parameters.portalPageId?has_content>&portalPageId=${rawString(parameters.portalPageId!)}</#if></#assign>
          <li class="has-form"><@modal label=uiLabelMap.CommonHelp id="help" href=makeOfbizUrl("showHelp?helpTopic=${rawString(helpTopic!)}${portalPageParamStr}")></@modal></li>      
        </ul>
        
        <#if userLogin?has_content>
            <ul class="left">
              <li class="has-dropdown not-click"><a href="#">${uiLabelMap["CommonApplications"]}</a>
                <ul class="dropdown">
                  <li class="has-dropdown not-click"><a href="#">${uiLabelMap["CommonPrimaryApps"]}</a>
                    <ul class="dropdown">
                      <@primaryAppsMenu/>
                    </ul>
                  </li>
                  <li class="has-dropdown not-click"><a href="#">${uiLabelMap["CommonSecondaryApps"]}</a>
                    <ul class="dropdown">
                      <@secondaryAppsMenu/>
                    </ul>
                  </li>                  
                </ul>
              </li>
        </#if>
