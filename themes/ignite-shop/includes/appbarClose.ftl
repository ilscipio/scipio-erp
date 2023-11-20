        <#-- Macro for rendering the sidebar. Relies on a tiny screenwidget that we are using for rendering menus-->
        <#macro sideBarMenu>
            <#if mainSideBarMenuCfg?has_content && mainSideBarMenuCfg.location?has_content && mainSideBarMenuCfg.name?has_content>
                <#-- TODO: REVIEW: Leaving this here for now as it might not currently matter for shop, but see
                    component://ignite-admin/includes/appbarClose.ftl for discussion; substitute if issues
                <@render type="menu" name=mainSideBarMenuCfg.name resource=mainSideBarMenuCfg.location subMenus="current"/> -->
                <@render type="menu" name=mainSideBarMenuCfg.name resource=mainSideBarMenuCfg.location subMenus="all"/>
            <#elseif sections??>
                ${sections.render("left-column")}
            </#if>
        </#macro>
        <div class="app-body">
            <#--<div class="sidebar">
                <@sideBarMenu/> 
            </div>-->
            <main class="main">