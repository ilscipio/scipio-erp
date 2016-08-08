<#if security.hasEntityPermission("CATALOG", "_VIEW", session)>
    <@treemenu type="lib-model" id=productStoreId data=treeMenuData settings=treeMenuSettings plugins=treeMenuPlugins>
        <@treemenu_event event="activate_node.jstree">
            id = data.node.id;
            if (data.node.li_attr.original_id && data.node.li_attr.original_id != id)
                id = data.node.li_attr.original_id;
            if(data.node.type == "catalog") {
                URL = 'EditProdCatalog';
                dataSet = {"prodCatalogId" : id};
            } else if(data.node.type == "category") {
                URL = 'EditCategory';
                dataSet = {"productCategoryId" : id};
            } else if(data.node.type == "product") {
                URL = 'ViewProduct';
                dataSet = {"productId" : id};
            }
            if (URL && dataSet) {
                window.location = URL + '?' + $.param(dataSet);
                
            }
        </@treemenu_event>
    </@treemenu>
    <@modal id=productStoreId />
</#if>
