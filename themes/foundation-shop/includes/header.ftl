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
<#if (requestAttributes.externalLoginKey)??><#assign externalKeyParam = "?externalLoginKey=" + requestAttributes.externalLoginKey!></#if>
<#if externalLoginKey??><#assign externalKeyParam = "?externalLoginKey=" + requestAttributes.externalLoginKey!></#if>
<#assign ofbizServerName = application.getAttribute("_serverId")!"default-server">
<#assign contextPath = request.getContextPath()>
<#if person?has_content>
  <#assign userName = person.firstName! + " " + person.middleName! + " " + person.lastName!>
<#elseif partyGroup?has_content>
  <#assign userName = partyGroup.groupName!>
<#elseif userHasAccount><#-- NOTE: see common.ftl for userHasAccount setup -->
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
    <#if userHasAccount>
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
        <li><a href="<@ofbizUrl>orderhistory</@ofbizUrl>">${uiLabelMap.CommonOrders}</a></li><#--uiLabelMap.EcommerceOrderHistory-->
        <#-- TODO: Ofbiz/ecommerce supports more (above are bare essentials only):
        <li><a href="<@ofbizUrl>messagelist</@ofbizUrl>">${uiLabelMap.CommonMessages}</a></li>
        <li><a href="<@ofbizUrl>ListQuotes</@ofbizUrl>">${uiLabelMap.OrderOrderQuotes}</a></li>
        <li><a href="<@ofbizUrl>ListRequests</@ofbizUrl>">${uiLabelMap.OrderRequests}</a></li>
        <li><a href="<@ofbizUrl>editShoppingList</@ofbizUrl>">${uiLabelMap.EcommerceShoppingLists}</a></li>
        -->
        <li><a href="<@ofbizUrl>viewprofile</@ofbizUrl>">${uiLabelMap.CommonProfile}</a></li>

        <#-- not implemented for shop, belongs to profile settings: <li><a href="<@ofbizUrl>ListLocales</@ofbizUrl>">${uiLabelMap.CommonLanguageTitle}</a></li>-->
        <#-- not implemented for shop: <li><a href="<@ofbizUrl>ListVisualThemes</@ofbizUrl>">${uiLabelMap.CommonVisualThemes}</a></li>-->
    <#else>
        <#-- language select for anon users 
            MOVED to icon
        <li><a href="<@ofbizUrl>ListLocales</@ofbizUrl>">${uiLabelMap.CommonLanguageTitle}</a></li> -->
    </#if>
    <#--
    <#if parameters.componentName?? && requestAttributes._CURRENT_VIEW_?? && helpTopic??>
        <#include "component://common/webcommon/includes/helplink.ftl" />
    </#if>-->
    <#if userHasAccount>
        <li class="divider"></li>
    </#if>
    <#-- Now show this even for anon, unless it's anon without a party -->
    <#if userIsKnown>
        <li class="active"><a href="<@ofbizUrl>logout</@ofbizUrl>"<#-- class="alert ${styles.link_nav!}"-->>${uiLabelMap.CommonLogout}</a></li>
    </#if>
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
<body class="<#if activeApp?has_content>app-${activeApp}</#if><#if parameters._CURRENT_VIEW_?has_content> page-${parameters._CURRENT_VIEW_!}</#if> page-auth">
<#-- ================================
            SOCIAL LOGIN 
     ================================
-->
<#-- Facebook Authentication Addon (required)-->
<#if getPropertyMsg("shop.properties","facebook.enabled")== "Y">
    <#include "component://auth-facebook/webapp/facebook/fb-common.ftl"/>
    <@fbInit scope="public_profile,email"/>
</#if>
<#-- Google Authentication Addon (required)-->
<#if getPropertyMsg("shop.properties","google.enabled")== "Y">
    <#include "component://auth-google/webapp/google/google-common.ftl"/>
    <@googleInit/>
</#if>
<#-- Twitter Authentication Addon (required)-->
<#if getPropertyMsg("shop.properties","twitter.enabled")== "Y">
    <#include "component://auth-twitter/webapp/facebook/fb-twitter.ftl"/>
    <@twitterInit/>
</#if>
<#-- ================================ -->
<div class="header" id="header">
</div>

<#macro rightMenu>
      <#-- SCIPIO: NOTE: We must display something for the anonymous user that has a partyId
          attached (created during anon checkout), because otherwise he has no way to clear his session.
          His temporary partyId is now (and must be) kept after checkout is done, for technical reasons,
          but also it's very convenient. 
          Presence of userLogin.partyId is what marks the difference. -->
      <#if userIsKnown>
          <li class="has-dropdown not-click">
            <#if userIsAnon>
              <#assign person = delegator.findOne("Person", {"partyId":userLogin.partyId}, true)!>
              <#if person?has_content>
                <#assign welcomeName = person.firstName!userLogin.userLoginId>
              <#else>
                <#assign partyGroup = delegator.findOne("PartyGroup", {"partyId":userLogin.partyId}, true)!>
                <#if partyGroup?has_content>
                  <#assign welcomeName = partyGroup.groupName!userLogin.userLoginId>
                <#else>
                  <#-- Use userLoginId ("anonymous") as the fallback for now; the partyId will be a random number, kind of insulting -->
                  <#assign welcomeName = userLogin.userLoginId>
                </#if>
              </#if>
            <#else>
              <#-- NOTE: This is a bit inconsistent with the anon user -->
              <#assign welcomeName = userLogin.userLoginId>
            </#if>
            <a href="#">${uiLabelMap.CommonWelcome}! ${welcomeName}</a>
            <ul class="dropdown">       
                <@generalMenu />
            </ul>
          </li>
      <#else>
        <li>
            <a href="<@ofbizUrl>${checkLoginUrl}</@ofbizUrl>">${uiLabelMap.CommonLogin}</a>
        </li>
      </#if>
      
      
</#macro>

<div class="off-canvas-wrap" data-offcanvas id="body-content">
<div class="inner-wrap">

  <#assign showHeadActn = (showHeaderActions!true) == true && (useMinimalTheme!false) == false>

    <!-- Off Canvas Menu -->
  <#if showHeadActn>
    <aside class="right-off-canvas-menu">
        <!-- whatever you want goes here -->
        <ul class="off-canvas-list">
            <@rightMenu/>
        </ul>
    </aside>
  </#if>
    
    <nav class="tab-bar show-for-small">
      <#if showHeadActn>
        <section class="left-small">
            <@render resource="component://shop/widget/CartScreens.xml#microcart" />
        </section>
      </#if>
        <section class="middle tab-bar-section">
            <h1><@logoMenu isSmall=true/></h1>
        </section>
      <#if showHeadActn>
        <section class="right-small">
            <a class="right-off-canvas-toggle menu-icon"><span></span></a>
        </section>
      </#if>
    </nav>
    
    <div class="sticky">   
    <nav class="top-bar hide-for-small" data-topbar role="navigation" data-options="sticky_on: large">
      <ul class="title-area">
        <li class="name">
            <h1><@logoMenu isSmall=true/></h1>   
          </li>
      </ul>
    
      <section class="top-bar-section">
      <#if showHeadActn>
        <!-- Right Nav Section -->
        <ul class="right">
          <li class="has-form">
            <@render resource="component://shop/widget/CatalogScreens.xml#keywordsearchbox" />
          </li>
          <li class="divider"></li>
          <@rightMenu/>
          <li class="divider"></li>
          <@render resource="component://shop/widget/CartScreens.xml#microcart" ctxVars={"microCartMenuItem":true}/>
          <#--
          <#assign portalPageParamStr><#if parameters.portalPageId?has_content>&portalPageId=${rawString(parameters.portalPageId!)}</#if></#assign>
          <li class="has-form"><@modal label=uiLabelMap.CommonHelp id="help" href=makeOfbizUrl("showHelp?helpTopic=${rawString(helpTopic!)}${portalPageParamStr}")></@modal></li> 
          <#-- language select
          <li>
            <div id="lang-select">
              <a href="<@ofbizUrl><#if userHasAccount>viewprofile<#else>ListLocales</#if></@ofbizUrl>">
                <i class="${styles.icon} ${styles.icon_prefix}flag"></i>
              </a>
            </div>
          </li>
          --> 
        </ul>
      </#if>
      </section>
    
      
