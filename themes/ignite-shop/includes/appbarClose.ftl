        <#-- Macro for rendering the sidebar. Relies on a tiny screenwidget that we are using for rendering menus-->
        <#macro sideBarMenu>
            <#if mainSideBarMenuCfg?has_content && mainSideBarMenuCfg.location?has_content && mainSideBarMenuCfg.name?has_content>
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