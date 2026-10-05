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
    Aurora shell, part 2 of 3.
    header.ftl opened the rail and the app panel with its <nav>. This file renders the
    application menu into it, closes the panel, renders the top bar and the jump dialog,
    and opens the content area that footer.ftl closes.
-->
<#macro sideBarMenu>
    <#-- NOTE: forced to use global vars because ctxVars suffer from backward-nesting issues with type="section" -->
    <@render type="section" name="left-column" globalCtxVars={"menuCfgSubMenuFilter":"all", "menuCfgItemCondMode":"disable-with-submenu"}/>
</#macro>

<#if userLogin??>
    <#assign pageTitle = "">
    <#if title?has_content><#assign pageTitle = title><#elseif titleProperty?has_content><#assign pageTitle = uiLabelMap[titleProperty]></#if>
    <#assign hasSide = !(auNoSideColumn!false)>

    <#if hasSide>
                <@virtualSection name="Global-Column-Left">
                    <@sideBarMenu/>
                </@virtualSection>
                <p class="au-menu-empty" hidden>${uiLabelMap.CommonNoMenuMatch}</p>
                </nav>
            </aside>
        </div>
        <div class="au-scrim" data-au-side-close="true" hidden></div>
    </#if>

        <div class="au-main">
            <header class="au-bar">
                <button type="button" class="au-icon-button au-menu-button" data-au-side-toggle="true"
                    aria-controls="au-side" aria-expanded="true" aria-label="${uiLabelMap.CommonMenu}">
                    <i class="fa fa-bars" aria-hidden="true"></i>
                </button>
                <a class="au-bar-brand" href="<@pageUrl>main</@pageUrl>">
                    <img src="<@ofbizContentUrl>/aurora/images/scipio-logo-small.svg</@ofbizContentUrl>" alt="" width="22" height="26"/>
                    <span>${applicationTitle!}</span>
                </a>
                <nav class="au-crumbs" aria-label="${uiLabelMap.CommonNavigation}">
                    <a href="<@pageUrl>main</@pageUrl>">${applicationTitle!}</a>
                    <#if pageTitle?has_content && pageTitle != (applicationTitle!"")>
                        <i class="fa fa-angle-right au-crumb-sep" aria-hidden="true"></i>
                        <span class="au-crumb-page" aria-current="page">${pageTitle}</span>
                    </#if>
                </nav>

                <div class="au-tools">
                    <button type="button" class="au-jump" data-au-jump-open="true" aria-haspopup="dialog" aria-controls="au-jump">
                        <i class="fa fa-search" aria-hidden="true"></i>
                        <span class="au-jump-label">${uiLabelMap.CommonJumpTo}</span>
                        <kbd class="au-kbd">Ctrl K</kbd>
                    </button>
                    <#if systemNotifications?has_content>
                        <div class="au-pop">
                            <button type="button" class="au-icon-button au-bell" aria-expanded="false" aria-controls="au-notes" data-au-popover="au-notes"
                                aria-label="${uiLabelMap.CommonNotifications}<#if systemNotificationsCount?has_content>, ${systemNotificationsCount}</#if>">
                                <i class="fa fa-bell-o" aria-hidden="true"></i>
                                <#if systemNotificationsCount?has_content><span class="au-dot" aria-hidden="true"></span></#if>
                            </button>
                            <div class="au-pop-panel au-notes" id="au-notes" hidden>
                                <p class="au-pop-heading">${uiLabelMap.CommonLastSytemNotes}</p>
                                <ul>
                                <#list systemNotifications as notification>
                                    <#assign notificationUrl = "#">
                                    <#if notification.url?has_content><#assign notificationUrl = addParamsToUrl(notification.url, {"scipioSysMsgId":notification.messageId})></#if>
                                    <li class="<#if (notification.isRead!"") == "Y">is-read</#if>">
                                        <a href="${notificationUrl}">
                                            <span class="au-note-title">${notification.title!"-"}</span>
                                            <span class="au-note-time">${notification.createdStamp?string.short}</span>
                                            <#if notification.description?has_content><span class="au-note-body">${notification.description}</span></#if>
                                        </a>
                                    </li>
                                </#list>
                                </ul>
                            </div>
                        </div>
                    </#if>
                    <button type="button" class="au-icon-button au-scheme-switch" data-au-scheme-toggle="true"
                        data-au-pref-url="<@pageUrl>ajaxSetUserPreference</@pageUrl>"
                        title="${uiLabelMap.CommonLightDark}" aria-label="${uiLabelMap.CommonLightDark}">
                        <#-- Line icons: the Font Awesome 4 sun reads as a cog. -->
                        <svg class="au-icon-dark" width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.8" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true" focusable="false"><path d="M20 14.5A8 8 0 1 1 9.5 4a6.5 6.5 0 0 0 10.5 10.5z"/></svg>
                        <svg class="au-icon-light" width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.8" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true" focusable="false"><circle cx="12" cy="12" r="4"/><path d="M12 2.5v2M12 19.5v2M2.5 12h2M19.5 12h2M5.3 5.3l1.4 1.4M17.3 17.3l1.4 1.4M5.3 18.7l1.4-1.4M17.3 6.7l1.4-1.4"/></svg>
                    </button>
                </div>
            </header>

            <div class="au-jump-dialog" id="au-jump" role="dialog" aria-modal="true" aria-label="${uiLabelMap.CommonJumpTo}" hidden>
                <div class="au-jump-box">
                    <div class="au-jump-field">
                        <i class="fa fa-search" aria-hidden="true"></i>
                        <input type="search" class="au-jump-input" autocomplete="off" placeholder="${uiLabelMap.CommonJumpTo}" aria-label="${uiLabelMap.CommonJumpTo}" aria-controls="au-jump-list"/>
                        <kbd class="au-kbd">Esc</kbd>
                    </div>
                    <ul class="au-jump-list" id="au-jump-list" role="listbox" aria-label="${uiLabelMap.CommonJumpTo}"></ul>
                    <p class="au-jump-empty" hidden>${uiLabelMap.CommonNoMenuMatch}</p>
                </div>
            </div>
            <main class="scp-content" id="au-content">
<#else>
        <div class="au-main">
            <main class="scp-content" id="au-content">
</#if>
