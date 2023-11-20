<#--<#include "component://base-theme/includes/appbarClose.ftl">-->
<#macro sideBarMenu>
    <#--<#if (mainSideBarMenuCfg.location)?has_content && (mainSideBarMenuCfg.name)?has_content>
                <@render type="menu" name=mainSideBarMenuCfg.name resource=mainSideBarMenuCfg.location subMenus="all"/>
            </#if-->
    <#-- NOTE: forced to use global vars because ctxVars suffer from backward-nesting issues with type="section" -->
    <#-- TODO: Currently this is not providing enough benefit and generates still too many broken links, so
        until menus are globally refactored across all apps and menu items parameters are re-standardized (using application-wide-scope
        context variables), it leads to too many menu expansions having seriously broken links leading users to broken pages,
        because visible menus are force-expanded even when all the links are missing required parameters, which happens because
        expansions were previously based on the current screen only; essentially all the menu-item and sub-menu conditions
        need to become current-screen-agnostic and the context variables used to populate their parameters must be
        standardized globally per-application
    <@render type="section" name="left-column" globalCtxVars={"menuCfgSubMenuFilter":"all"}/>-->
    <@render type="section" name="left-column" globalCtxVars={"menuCfgSubMenuFilter":"current"}/>
</#macro>

<#-- Sets sidebar based on cookie value -->
<#assign cookies = request.getCookies()!/>
<#assign hasSideBarActive = false />
<#if cookies?has_content>
    <#assign cookieName = "scpSidebar">
    <#list cookies?filter(x -> cookieName == x.name) as x>
        <#if x.value?has_content>
            <#assign hasSideBarActive = (x.value)?boolean />
        </#if>
    </#list>
</#if>

<div class="scp-container<#if hasSideBarActive> is-active</#if>" id="scpwrap">
    <#if userLogin??>
        <div class="scp-sidebar is-hidden-touch pb-0 is-narrow-mobile is-narrow hero is-fullheight is-flex is-flex-direction-column is-justify-content-space-between">
            <div>
                <div class="sidebar-toggle<#if hasSideBarActive> is-active</#if>" aria-label="expand sidebar" aria-expanded="false" data-target="scpwrap"><i class="fa fa-angle-double-left"></i></div>
                <@virtualSection name="Global-Column-Left">
                    <#if htmlwrap?has_content><${htmlwrap} class="menu-list"<#if id?has_content> id="menu_logo"</#if><#if style?has_content> style="${escapeVal(style, 'html')}"</#if><#if attribs?has_content><@commonElemAttribStr attribs=attribs exclude=excludeAttribs/></#if>></#if>
                    <@sideBarMenu/>
                </@virtualSection>
            </div>
            <footer class="footer">
                <div class="content has-text-centered">
                    <a href="https://www.ilscipio.com" target="_blank">&copy; ilscipio</a>
                </div>
            </footer>
        </div>
    </#if>
    <div class="scp-content">
