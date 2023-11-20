        <#-- Macro for rendering the sidebar. Relies on a tiny screenwidget that we are using for rendering menus-->
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
        <div class="app-body">
            <#if userLogin??>
                <div class="sidebar">
                  <@virtualSection name="Global-Column-Left">
                    <@sideBarMenu/>
                  </@virtualSection>
                <#-- Activate for additional resizing option
                <button class="sidebar-minimizer brand-minimizer" type="button"></button>-->
                </div>
            </#if>
            <main class="main">