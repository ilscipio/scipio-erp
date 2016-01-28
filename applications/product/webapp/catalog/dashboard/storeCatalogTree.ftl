<#if security.hasEntityPermission("CATALOG", "_VIEW", session)>    
    <@treemenu id=productStoreId data=treeMenuData settings=treeMenuSettings />
</#if>