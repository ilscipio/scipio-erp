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
<#--


********************
*       METRO      *
********************

 -->
<#-- Variables -->
<#if (requestAttributes.externalLoginKey)??><#assign externalKeyParam = "?externalLoginKey=" + (requestAttributes.externalLoginKey!)></#if>
<#if externalLoginKey??><#assign externalKeyParam = "?externalLoginKey=" + (requestAttributes.externalLoginKey!)></#if>
<#assign ofbizServerName = application.getAttribute("_serverId")!"default-server">
<#assign contextPath = request.getContextPath()>
<#if userLogin?has_content>
    <#assign displayApps = Static["org.ofbiz.webapp.control.LoginWorker"].getAppBarWebInfos(security, userLogin, ofbizServerName, "main")>
    <#assign displaySecondaryApps = Static["org.ofbiz.webapp.control.LoginWorker"].getAppBarWebInfos(security, userLogin, ofbizServerName, "secondary")>
    <#assign appModelMenu = Static["org.ofbiz.widget.model.MenuFactory"].getMenuFromLocation(applicationMenuLocation,applicationMenuName)>
</#if>
<#if person?has_content>
  <#assign userName = person.firstName!"" + " " + person.middleName!"" + " " + person.lastName!"">
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

<#-- Macro for rendering the general menu (userprofile etc.) -->
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
    <#else>
        <#assign portalPageParamStr><#if parameters.portalPageId?has_content>&portalPageId=${rawString(parameters.portalPageId!)}</#if></#assign>
        <li class="has-form"><@modal label=uiLabelMap.CommonHelp id="help" href=makeOfbizUrl("showHelp?helpTopic=${rawString(helpTopic!)}${portalPageParamStr}")></@modal></li> 
    </#if>
    <#if userLogin??>
        <li class="divider"></li>
        <li class="active"><a href="<@ofbizUrl>logout</@ofbizUrl>"<#-- class="alert ${styles.link_nav!}"-->>${uiLabelMap.CommonLogout}</a></li>
    </#if>
</#macro>

<#-- Macro for rendering the primary applications in a list -->
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

<#-- Macro for rendering the secondary applications in a list -->
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

<#-- Macro for rendering the sidebar. Relies on a tiny screenwidget that we are using for rendering menus-->
<#macro sideBarMenu>
    <#--<#if applicationMenuLocation?has_content && applicationMenuName?has_content>
        <@render type="menu" name=applicationMenuName resource=applicationMenuLocation />
    </#if>-->
  <#if sections??>
    ${sections.render("left-column")}
  </#if>
</#macro>

<#-- Macro for rendering your company logo. Uses a smaller version of your logo if isSmall=true. -->
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
            <#if hasLink><a href="<@ofbizUrl>${logoLinkURL}</@ofbizUrl>"></#if><img alt="${layoutSettings.companyName}" src="<@ofbizContentUrl>${rawString(organizationLogoLinkURL)}</@ofbizContentUrl>"/><span class="logo-text">${applicationTitle!}</span><#if hasLink></a></#if>
            <#else><#if hasLink><a href="<@ofbizUrl>${logoLinkURL}</@ofbizUrl>"></#if><img alt="${layoutSettings.companyName}" src="<@ofbizContentUrl>${rawString(headerImageUrl)}</@ofbizContentUrl>"/><span class="logo-text">${applicationTitle!}</span><#if hasLink></a></#if>
        </#if>
        <#else>
        <a href="<@ofbizUrl>${logoLinkURL!""}</@ofbizUrl>"><img alt="${layoutSettings.companyName}" src="<@ofbizContentUrl>/images/scipio/<#if isSmall>scipio-logo-small.png<#else>scipio-logo.svg</#if></@ofbizContentUrl>"/><span class="logo-text">${applicationTitle!}</span></a>
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
                    <#-- Sidebar -->
                    
                    <nav class="side-bar show-for-large-up">
                        <!-- Profile -->
                        <div class="title-area">
                            <div class="name">
                                <h1><@logoMenu isSmall=true/></h1>
                              </div>
                          </div>
                        <!-- End of Profile -->

                        <!-- Menu sidebar begin-->
                        <#-- NOTE: side-nav is on the child ul -->
                        <div class="side-nav-area">
                        <#if userLogin??>  
                            <@sideBarMenu/> 
                        </#if>                
                        </div>
                        
                    </nav>
                    
                    
                    <#-- Topbar -->
                    <nav class="top-bar show-for-large-up" data-topbar role="navigation" data-options="is_hover: false">
                        <ul class="title-area left">
                            <!-- Remove the class "menu-icon" to get rid of menu icon. Take out "Menu" to just have icon alone -->
                            <li class="toggle-topbar menu-icon"><a href="#"><span>#</span></a></li>
                        </ul>

                        <section class="top-bar-section ">
                            <!-- Right Nav Section -->
                            <#if userLogin??>
                            <ul class="left">
                                <li class="has-dropdown">
                                    <a class="" href="#"><i class="fi-home text-blue"></i> ${uiLabelMap["CommonPrimaryApps"]}</a>
                                     <ul class="dropdown">
                                        <@primaryAppsMenu/>
                                    </ul>
                                </li>
                                <li class="has-dropdown">
                                    <a class="" href="#"><i class="fi-widget text-green"></i> ${uiLabelMap["CommonSecondaryApps"]}</a>
                                    <ul class="dropdown">
                                        <@secondaryAppsMenu/>
                                    </ul>
                                </li>
                            </ul>
                            </#if>

                            <#-- Left Nav Section
                            <ul class="left">

                                <li class="has-form bg-white">
                                    <div class="row collapse">

                                        <div class="large-12 columns">
                                            <div class="dark"> </div>
                                            <input class="input-top" type="text" placeholder="search">
                                        </div>
                                    </div>
                                </li>
                            </ul>-->

                            <ul class="right">
                                <li class="has-dropdown">
                                   <#if userLogin??><a class="" href="#"><i class="fi-torso"></i> ${uiLabelMap.CommonWelcome}! ${userLogin.userLoginId}</a><#else><a href="<@ofbizUrl>${checkLoginUrl}</@ofbizUrl>">${uiLabelMap.CommonLogin}</a></#if>
                                    <ul class="dropdown">
                                        <@generalMenu />
                                    </ul>
                                </li>
                            </ul>
                        </section>
                    </nav>
                        
                    <#-- Off-Canvas Menus -->    
                    <aside class="right-off-canvas-menu">
                        <ul class="off-canvas-list">
                          <@generalMenu />
                          <#assign portalPageParamStr><#if parameters.portalPageId?has_content>&portalPageId=${rawString(parameters.portalPageId!)}</#if></#assign>
                          <li class="has-form"><@modal label=uiLabelMap.CommonHelp id="help" href=makeOfbizUrl("showHelp?helpTopic=${rawString(helpTopic!)}${portalPageParamStr}")></@modal></li>  
                        </ul>
                    </aside>
                    
                    <#if userLogin??>  
                    <aside class="left-off-canvas-menu">
                      <ul class="off-canvas-list">
                          <@sideBarMenu/> 
                          <@primaryAppsMenu/>
                          <@secondaryAppsMenu/>
                       </ul>
                    </aside>
                    </#if>
                
                    <nav class="tab-bar hide-for-large-up">
                        <#if userLogin??>
                            <section class="left-small">
                                <a class="left-off-canvas-toggle menu-icon"><span></span></a>
                            </section>
                        </#if>
                        <section class="middle tab-bar-section">
                            <h1><@logoMenu isSmall=true/></h1>
                        </section>
                        <section class="right-small">
                            <a class="right-off-canvas-toggle menu-icon"><span></span></a>
                        </section>
                    </nav>