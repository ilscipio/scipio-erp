<#if security.hasEntityPermission("CATALOG", "_VIEW", session)>    
    <@treemenu id=productStoreId data=treeData/>
</#if>