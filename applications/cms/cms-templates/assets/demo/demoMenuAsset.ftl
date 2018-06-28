<#-- Demo menu. -->
<@menu type=(type!"button")><#--  items=(items![]) args=(menuArgs!{}) -->
    <#assign linkUrl><@pageUrl name="BasicTemplateExample" webSiteId="cmsSite"/></#assign>
    <@menuitem type="link" href=linkUrl text="Basic Template Example" class="+${styles.action_nav}"/>
    
    <#assign linkUrl><@pageUrl name="AdvancedTemplateGlobalExample" webSiteId="cmsBackendSite"/></#assign>
    <@menuitem type="link" href=linkUrl text="Advanced Template Global Example" class="+${styles.action_nav}"/>
    
    <#assign linkUrl><@pageUrl name="AdvancedTemplateShopExample" webSiteId="cmsSite"/></#assign>
    <@menuitem type="link" href=linkUrl text="Advanced Template Shop Example" class="+${styles.action_nav}"/>
    
    <#assign linkUrl><@pageUrl name="DemoPage" webSiteId="cmsSite"/></#assign>
    <@menuitem type="link" href=linkUrl text="DemoPage" class="+${styles.action_nav}"/>
</@menu>
