<#if security.hasEntityPermission("CATALOG", "_VIEW", session)>    
    <@treemenu id="testTree" data=treeData/>
</#if>