<#--
Scipio Commerce
Copyright (C) Ilscipio GmbH

This file is part of Scipio Commerce. Scipio Commerce is free software: you
can redistribute it and modify it under the terms of the GNU Affero General
Public License, version 3, as published by the Free Software Foundation.
Scipio Commerce is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
for more details. You should have received a copy of the license with this
work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
A commercial license is available from Ilscipio GmbH.

SPDX-License-Identifier: AGPL-3.0-only
-->
<#--
    Aurora shell, part 1 of 3.
    header.ftl      - the head, the body tag, the application rail, the app panel up to its menu
    appbarClose.ftl - the app menu, the top bar, the jump dialog, opens the content area
    footer.ftl      - closes the content area, the footer scripts
-->
<@virtualSection name="Global-Header-Ignite" contains="!$Global-Column-Left, *">
    <#assign externalKeyParam = "">
    <#if (requestAttributes.externalLoginKey)?has_content><#assign externalKeyParam = "?externalLoginKey=" + raw(requestAttributes.externalLoginKey)></#if>
    <#assign ofbizServerName = application.getAttribute("_serverId")!"default-server">
    <#assign contextPath = request.getContextPath()>
    <#assign displayApps = []>
    <#assign displaySecondaryApps = []>
    <#if userLogin?has_content>
        <#assign displayApps = Static["org.ofbiz.webapp.control.LoginWorker"].getAppBarWebInfos(security, userLogin, ofbizServerName, "main")>
        <#assign displaySecondaryApps = Static["org.ofbiz.webapp.control.LoginWorker"].getAppBarWebInfos(security, userLogin, ofbizServerName, "secondary")>
    </#if>
    <#assign logoUrl = makeOfbizContentUrl("/aurora/images/scipio-logo-small.svg")>

    <#-- The app panel is hidden on a wide screen when the reader closed it; the cookie
         lets the server render that state, so the page does not jump. -->
    <#assign sideHidden = false>
    <#list (request.getCookies())![] as auCookie>
        <#if auCookie.name == "scpSidebar" && (auCookie.value!"") == "true"><#assign sideHidden = true></#if>
    </#list>

    <#assign userName = "">
    <#if person?has_content><#assign userName = ((person.firstName!"") + " " + (person.lastName!""))?trim>
    <#elseif partyGroup?has_content><#assign userName = partyGroup.groupName!""></#if>
    <#if !userName?has_content && userLogin??><#assign userName = userLogin.userLoginId></#if>
    <#assign userInitials = "">
    <#list userName?split(" ") as part><#if part?has_content && userInitials?length < 2><#assign userInitials = userInitials + part?substring(0, 1)?upper_case></#if></#list>

    <#-- One application in the rail. -->
    <#macro auRailItem display>
        <#local thisApp = display.getContextRoot()>
        <#local selected = (thisApp == contextPath || contextPath + "/" == thisApp)>
        <#local servletPath = Static["org.ofbiz.webapp.WebAppUtil"].getControlLinkPathSafeSlash(display)!"">
        <#local thisURL = raw(servletPath)>
        <#if thisApp != "/">
            <#if servletPath?has_content><#local thisURL = thisURL + "main"><#else><#local thisURL = thisApp></#if>
        </#if>
        <#local appTitle = (uiLabelMap[display.title])!display.title>
        <a class="au-rail-item<#if selected> is-current</#if>" href="${thisURL}${externalKeyParam}" title="${appTitle}" aria-label="${appTitle}"<#if selected> aria-current="page"</#if>><i class="${styles.icon!} ${(styles.app_icon[display.name])!'fa-folder-o'}" aria-hidden="true"></i></a>
    </#macro>

    <#-- The application rail: the logo, every application the reader may open, the reader. -->
    <#macro auRail>
        <nav class="au-rail" aria-label="${uiLabelMap.CommonApplications}">
            <a class="au-rail-logo" href="/${externalKeyParam}" title="${uiLabelMap.CommonApplications}" aria-label="${uiLabelMap.CommonApplications}"><img src="${logoUrl}" alt="" width="30" height="35"/></a>
            <div class="au-rail-apps">
                <#list displayApps as display><@auRailItem display=display/></#list>
                <#if displaySecondaryApps?has_content>
                    <span class="au-rail-sep" aria-hidden="true"></span>
                    <#list displaySecondaryApps as display><@auRailItem display=display/></#list>
                </#if>
            </div>
            <div class="au-rail-foot au-pop">
                <button type="button" class="au-avatar" aria-expanded="false" aria-controls="au-user" data-au-popover="au-user" title="${userName}" aria-label="${userName}">${userInitials}</button>
                <div class="au-pop-panel au-user-menu" id="au-user" hidden>
                    <p class="au-pop-heading"><strong>${userName}</strong><br/>${userLogin.userLoginId}</p>
                    <ul>
                        <li><a href="<@pageUrl>ListVisualThemes</@pageUrl>"><i class="fa fa-paint-brush" aria-hidden="true"></i> ${uiLabelMap.CommonVisualThemes}</a></li>
                        <li><a href="<@pageUrl>ListLocales</@pageUrl>"><i class="fa fa-language" aria-hidden="true"></i> ${uiLabelMap.CommonLanguageTitle}</a></li>
                        <li class="au-sep"><a href="<@pageUrl>logout?t=${.now?long?c}</@pageUrl>"><i class="fa fa-sign-out" aria-hidden="true"></i> ${uiLabelMap.CommonLogout}</a></li>
                    </ul>
                </div>
            </div>
        </nav>
    </#macro>

    <#assign currentAppIcon = "fa-folder-o">
    <#list displayApps + displaySecondaryApps as display>
        <#if display.getContextRoot() == contextPath || contextPath + "/" == display.getContextRoot()>
            <#assign currentAppIcon = (styles.app_icon[display.name])!currentAppIcon>
        </#if>
    </#list>

    <@scripts output=true> <#-- ensure @script elems here will always output -->

        <title>${layoutSettings.companyName}<#if title?has_content>: ${title}<#elseif titleProperty?has_content>: ${uiLabelMap[titleProperty]}</#if></title>

        <#if layoutSettings.shortcutIcon?has_content>
            <#assign shortcutIcon = layoutSettings.shortcutIcon/>
        <#elseif layoutSettings.VT_SHORTCUT_ICON?has_content>
            <#assign shortcutIcon = layoutSettings.VT_SHORTCUT_ICON.get(0)/>
        </#if>
        <#if shortcutIcon?has_content>
            <link rel="shortcut icon" href="<@ofbizContentUrl>${raw(shortcutIcon)}</@ofbizContentUrl>" />
        </#if>

        <#-- The two faces of every page load before the style sheets ask for them. -->
        <link rel="preload" href="<@ofbizContentUrl>/aurora/fonts/geist-latin-wght-normal.woff2</@ofbizContentUrl>" as="font" type="font/woff2" crossorigin/>
        <link rel="preload" href="<@ofbizContentUrl>/aurora/fonts/bricolage-grotesque-latin-wght-normal.woff2</@ofbizContentUrl>" as="font" type="font/woff2" crossorigin/>

        <#if layoutSettings.styleSheets?has_content>
            <#--layoutSettings.styleSheets is a list of style sheets. So, you can have a user-specified "main" style sheet, AND a component style sheet.-->
            <#list layoutSettings.styleSheets as styleSheet>
                <link rel="stylesheet" href="<@ofbizContentUrl>${raw(styleSheet)}</@ofbizContentUrl>" type="text/css"/>
            </#list>
        </#if>
        <#if layoutSettings.VT_STYLESHEET?has_content>
            <#list layoutSettings.VT_STYLESHEET as styleSheet>
                <link rel="stylesheet" href="<@ofbizContentUrl>${raw(styleSheet)}</@ofbizContentUrl>" type="text/css"/>
            </#list>
        </#if>
        <#if layoutSettings.rtlStyleSheets?has_content && langDir == "rtl">
            <#--layoutSettings.rtlStyleSheets is a list of rtl style sheets.-->
            <#list layoutSettings.rtlStyleSheets as styleSheet>
                <link rel="stylesheet" href="<@ofbizContentUrl>${raw(styleSheet)}</@ofbizContentUrl>" type="text/css"/>
            </#list>
        </#if>
        <#if layoutSettings.VT_RTL_STYLESHEET?has_content && langDir == "rtl">
            <#list layoutSettings.VT_RTL_STYLESHEET as styleSheet>
                <link rel="stylesheet" href="<@ofbizContentUrl>${raw(styleSheet)}</@ofbizContentUrl>" type="text/css" />
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
                    ${raw(webAnalyticsConfig.webAnalyticsCode!)}
                </#list>
            </@script>
        </#if>

    </@scripts>
    </head>
    <body class="<#if activeApp?has_content>app-${activeApp}</#if><#if parameters._CURRENT_VIEW_?has_content> page-${parameters._CURRENT_VIEW_!}</#if> <#if userLogin??>page-auth<#else>page-noauth</#if>">
    <a class="au-skip" href="#content-main-section">${uiLabelMap.CommonSkipToContent}</a>
    <#if userLogin?has_content && (auNoSideColumn!false)>
    <#-- A page without an application menu (the launcher at "/"): the rail only. -->
    <div class="au-shell au-shell-wide" id="scpwrap">
        <div class="au-nav au-nav-rail-only" id="au-side"><@auRail/></div>
    <#elseif userLogin?has_content>
    <div class="au-shell<#if sideHidden> is-side-hidden</#if>" id="scpwrap">
        <div class="au-nav" id="au-side">
            <@auRail/>
            <aside class="au-panel" aria-label="${applicationTitle!}">
                <div class="au-panel-head">
                    <span class="au-app-tile" aria-hidden="true"><i class="${styles.icon!} ${currentAppIcon}"></i></span>
                    <span class="au-panel-title">${applicationTitle!}</span>
                    <button type="button" class="au-icon-button au-side-close" data-au-side-close="true" aria-label="${uiLabelMap.CommonClose}"><i class="fa fa-times" aria-hidden="true"></i></button>
                </div>
                <div class="au-menu-find">
                    <i class="fa fa-filter" aria-hidden="true"></i>
                    <input type="search" class="au-filter" data-au-filter="au-side-menu" autocomplete="off"
                        placeholder="${uiLabelMap.CommonFilterMenu}" aria-label="${uiLabelMap.CommonFilterMenu}"/>
                </div>
                <nav class="au-side-menu" id="au-side-menu" aria-label="${applicationTitle!}">
    <#else>
    <div class="au-shell au-shell-bare" id="scpwrap">
    </#if>
</@virtualSection>
