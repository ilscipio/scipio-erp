        <#-- Macro for rendering the sidebar. Relies on a tiny screenwidget that we are using for rendering menus-->
        <#macro sideBarMenu>
            <#--<#if (mainSideBarMenuCfg.location)?has_content && (mainSideBarMenuCfg.name)?has_content>
                <@render type="menu" name=mainSideBarMenuCfg.name resource=mainSideBarMenuCfg.location subMenus="all" itemCondMode="disable-with-submenu"/>
            </#if-->
            <#-- NOTE: forced to use global vars because ctxVars suffer from backward-nesting issues with type="section" -->
            <@render type="section" name="left-column" globalCtxVars={"menuCfgSubMenuFilter":"all", "menuCfgItemCondMode":"disable"}/>
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