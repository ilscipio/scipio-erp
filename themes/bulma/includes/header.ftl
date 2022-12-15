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
<@virtualSection name="Global-Header-Ignite" contains="!$Global-Column-Left, *">
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
    <#if layoutSettings.headerImageLinkUrl?has_content>
        <#assign logoLinkURL = "${layoutSettings.headerImageLinkUrl}">
    <#elseif layoutSettings.commonHeaderImageLinkUrl?has_content>
        <#assign logoLinkURL = "${layoutSettings.commonHeaderImageLinkUrl}">
    <#else>
        <#assign logoLinkURL = "/bulmatheme/images/scipio-logo.png">
    </#if>
    <#assign organizationLogoLinkURL = "${layoutSettings.organizationLogoLinkUrl!}">
    <#macro generalMenu>
        <#if userLogin??>
            <#--
        <#if layoutSettings.topLines?has_content>
          <#list layoutSettings.topLines as topLine>
            <#if topLine.text??>
              <li class="navbar-item">${topLine.text}<a href="${raw(topLine.url!)}${raw(externalKeyParam)}">${topLine.urlText!}</a></li>
            <#elseif topLine.dropDownList??>
              <li class="navbar-item"><#include "component://common/webcommon/includes/insertDropDown.ftl"/></li>
            <#else>
              <li class="navbar-item">${topLine!}</li>
            </#if>
          </#list>
        <#else>
          <li class="navbar-item">${userLogin.userLoginId}</li>
        </#if>
        -->
            <div><a href="<@ofbizUrl>ListVisualThemes</@ofbizUrl>" class="navbar-item"><i class="${styles.icon!} fa-photo mr-2"></i> ${uiLabelMap.CommonVisualThemes}</a></div>
        </#if>
        <div><a href="<@ofbizUrl>ListLocales</@ofbizUrl>" class="navbar-item"><i class="${styles.icon!} fa-language  mr-2"></i> ${uiLabelMap.CommonLanguageTitle}</a></div>
        <#-- Disabled Help function for the time being
    <#if parameters.componentName?? && requestAttributes._CURRENT_VIEW_?? && helpTopic??>
        <#include "component://common/webcommon/includes/helplink.ftl" />
        <#assign portalPageParamStr><#if parameters.portalPageId?has_content>&portalPageId=${raw(parameters.portalPageId!)}</#if></#assign>
        <li class="has-form"><@modal label=uiLabelMap.CommonHelp id="help" linkClass="navbar-item"
            href=makeOfbizUrl("showHelp?helpTopic=${raw(helpTopic!)}${portalPageParamStr}") icon="${styles.icon!} fa-info"></@modal></li>
    </#if>-->
        <#if userLogin??>
            <div class="active"><a href="<@ofbizUrl>logout?t=${.now?long}</@ofbizUrl>" class="navbar-item active"><i class="${styles.icon!} fa-power-off mr-2"></i> ${uiLabelMap.CommonLogout}</a></div>
        </#if>
    </#macro>

    <#macro primaryAppsMenu>
        <#assign appCount = 0>
        <#assign firstApp = true>
        <#--  <li class="navbar-item"><label>${uiLabelMap["CommonPrimaryApps"]}</label></li>-->
        <#list displayApps as display>
            <#assign thisApp = display.getContextRoot()>
            <#assign selected = false>
            <#if thisApp == contextPath || contextPath + "/" == thisApp>
                <#assign selected = true>
            </#if>
            <#assign servletPath = Static["org.ofbiz.webapp.WebAppUtil"].getControlServletPathSafeSlash(display)!"">
            <#assign thisURL = raw(servletPath)>
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
                    <a href="${thisURL}${raw(externalKeyParam)}" class="navbar-item<#if selected> active</#if>"
                    <#if uiLabelMap??> title="${uiLabelMap[display.description]}">
                        <#if styles.app_icon[display.name]?has_content><i class="mr-1 ${styles.icon!} ${styles.app_icon[display.name]}"></i> </#if>${uiLabelMap[display.title]}
                    <#else> title="${display.description}">
                    ${display.title}
                    </#if>
                    </a>
                <#assign appCount = appCount + 1>
            </#if>
        </#list>
    </#macro>

    <#macro secondaryAppsMenu>
        <#--<li class="navbar-item"><label>${uiLabelMap["CommonSecondaryApps"]}</label></li>-->
        <#if displaySecondaryApps?has_content>
            <#list displaySecondaryApps as display>
                <#assign thisApp = display.getContextRoot()>
                <#assign selected = false>
                <#if thisApp == contextPath || contextPath + "/" == thisApp>
                    <#assign selected = true>
                </#if>
                <#assign servletPath = Static["org.ofbiz.webapp.WebAppUtil"].getControlServletPathSafeSlash(display)!"">
                <#assign thisURL = raw(servletPath)>
                <#if thisApp != "/">
                    <#if servletPath?has_content>
                        <#assign thisURL = thisURL + "main">
                    <#else>
                        <#assign thisURL = thisApp>
                    </#if>
                </#if>
                    <a href="${thisURL}${raw(externalKeyParam)}" class="navbar-item <#if selected> active</#if>"
                    <#if uiLabelMap??> title="${uiLabelMap[display.description]}">
                        <#if styles.app_icon[display.name]?has_content><i class="mr-1 ${styles.icon!} ${styles.app_icon[display.name]}"></i> </#if>${uiLabelMap[display.title]}
                    <#else> title="${display.description}">
                        ${display.title}
                    </#if>
                    </a>
                    <#assign appCount = appCount + 1>
            </#list>
        </#if>
    </#macro>

    <#macro notificationsMenu>
        <div class="navbar-dropdown is-right">
            <div class="navbar-item">${uiLabelMap["CommonLastSytemNotes"]}</div>
            <hr class="navbar-divider" />
            <#list systemNotifications as notification>
                <div class="media-preview">
                    <#if notification.url?has_content><#assign notificationUrl=addParamsToUrl(notification.url,{"scipioSysMsgId":notification.messageId})></#if>
                    <a href="${notificationUrl!"#"}"  class="navbar-item">
                        <div class="message_wrap <#if notification.isRead?has_content && notification.isRead=='Y'>message_isread</#if>">
                            <#--<div class="message_status">
                            <#if notification.fromPartyId?has_content> <span class="message_user"><small>${notification.fromPartyId!""}</small></span></#if>
                        </div>-->
                            <div class="message_header">
                                ${notification.title!"No Title"} <span class="message_time float-right">${notification.createdStamp?string.short}</span>
                            </div>
                            <div class="message_body">${notification.description!""}</div>
                        </div>
                    </a>
                </div>
            </#list>
        </div>
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
        <div class="navbar-brand">
            <a role="button" class="navbar-burger" aria-label="open sidebar" aria-expanded="false" data-target="scpwrap">
                <span aria-hidden="true"></span>
                <span aria-hidden="true"></span>
                <span aria-hidden="true"></span>
            </a>
            <#if hasLink><a href="<@ofbizUrl>${logoLinkURL}</@ofbizUrl>" class="navbar-item brand-text is-hidden-desktop"></#if>
                <img alt="${layoutSettings.companyName}" src="<@contentUrl>/bulmatheme/images/scipio-logo-small.svg</@contentUrl>" width="150px"/>
            <#if hasLink></a></#if>
            <a role="button" class="navbar-burger" aria-label="applications and logout" aria-expanded="false" data-target="scpapps-mobile">
                <span aria-hidden="true"></span>
                <span aria-hidden="true"></span>
                <span aria-hidden="true"></span>
            </a>
        </div>
    </#macro>
    <meta name="viewport" content="width=device-width, initial-scale=1">

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
    <body class="<#if activeApp?has_content>app-${activeApp}</#if><#if parameters._CURRENT_VIEW_?has_content> page-${parameters._CURRENT_VIEW_!}</#if> <#if userLogin??>sidebar-fixed page-auth<#else>page-noauth</#if>">
    <!-- Navigation -->

    <nav class="app-header navbar" role="navigation" aria-label="main navigation">

        <@logoMenu isSmall=true/>

        <!-- Top Menu Items -->
        <#if userLogin?has_content>
            <div class="navbar-start navbar-menu">

                <div class="navbar-apps navbar-item" aria-expanded="false" aria-label="expand applications" data-target="scpapps">
                    <div>${applicationTitle!}</div>
                    <div class="navbar-apps-menu">
                        <div class="navbar-apps-bar" aria-hidden="true" ></div>
                        <div class="navbar-apps-bar" aria-hidden="true" ></div>
                    </div>
                </div>


            </div>

            <div class="navbar-apps-overlay " id="scpapps">
                <div class="navbar-wrap">
                    <@primaryAppsMenu/>
                    <@secondaryAppsMenu/>
                </div>
            </div>


            <div class="navbar-end navbar-menu">
                <#if systemNotifications?has_content>
                    <div class="navbar-item has-dropdown is-hoverable"><a href="#" class="navbar-link is-arrowless">
                        <div class="control">
                            <div class="tags has-addons">
                                <span class="tag is-dark is-medium"><i class="mr-1 ${styles.icon!} ${styles.icon_prefix!}bell"></i>
                                    <#if systemNotificationsCount?has_content> ${systemNotificationsCount}</span></#if>
                            </div>
                        </div>
                        </a>
                        <@notificationsMenu />
                    </div>
                <#else>

                                <div class="navbar-item">
                                    <div class="control">
                                        <div class="tags has-addons">
                                            <span class="tag is-success is-medium">${applicationTitle!}</span>
                                        </div>
                                    </div>
                                </div>
                </#if>
                <div class="navbar-item has-dropdown is-hoverable">
                    <#if userLogin??><a href="#" class="navbar-link" ><i class="mr-1 fa fa-user"></i> ${userLogin.userLoginId} </a><#else><a href="<@ofbizUrl>${checkLoginUrl}</@ofbizUrl>" class="navbar-link">${uiLabelMap.CommonLogin}</a></#if>
                    <div class="navbar-dropdown is-right">
                        <@generalMenu />
                    </div>
                </div>
            </div>
        </#if>
    </nav>

    <#-- mobile menu-->
    <#if userLogin?has_content>
        <nav class="navbar-menu is-hidden-desktop" id="scpapps-mobile">
            <!-- Top Menu Items -->
                <#if systemNotifications?has_content>
                    <div class="has-background-info">
                        <@notificationsMenu />
                    </div>
                    <hr class="navbar-divider">
                </#if>

                <div class="navbar-item has-dropdown is-hoverable">
                <#if userLogin??><a href="#" class="navbar-item" ><i class="mr-1 fa fa-user"></i> ${userLogin.userLoginId} </a><#else><a href="<@ofbizUrl>${checkLoginUrl}</@ofbizUrl>" class="navbar-item">${uiLabelMap.CommonLogin}</a></#if>
                <div class="navbar-dropdown is-right">
                    <@generalMenu />
                </div>
                <#if userLogin??>
                    <hr class="navbar-divider">
                    <div class="navbar-item has-dropdown is-hoverable">
                        <a href="javascript:;" class="navbar-item" ><i class="${styles.icon!} fa-dashboard"></i> <span class="ml-1">${uiLabelMap["CommonPrimaryApps"]}</span></a>
                        <div class="navbar-dropdown is-right">
                            <@primaryAppsMenu/>
                        </div>
                    </div>
                    <hr class="navbar-divider">
                    <div class="navbar-item has-dropdown is-hoverable">
                        <a href="javascript:;" class="navbar-item"><i class="${styles.icon!} fa-desktop"></i> <span class="ml-1">${uiLabelMap["CommonSecondaryApps"]}</span></a>
                        <div class="navbar-dropdown is-right">
                            <@secondaryAppsMenu/>
                        </div>
                    </div>
                </#if>

            </div>
        </nav>
    </#if>
</@virtualSection>
