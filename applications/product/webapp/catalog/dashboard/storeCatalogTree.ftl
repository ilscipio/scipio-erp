<#if security.hasEntityPermission("CATALOG", "_VIEW", session)>    
    <@treemenu id=productStoreId data=treeMenuData settings=treeMenuSettings plugins=treeMenuPlugins>
        <@treemenu_event event="activate_node.jstree">
            if(data.node.type == "catalog") {
                URL = 'EditProdCatalogAjax';
                dataSet = {"prodCatalogId" : data.node.id, "ajaxUpdateEvent" : "Y"};
            } else if(data.node.type == "category") {
                URL = 'EditCategoryAjax';
                dataSet = {"productCategoryId" : data.node.id, "ajaxUpdateEvent" : "Y"};
            }
            if (URL && dataSet) {
				var modal = $('#${productStoreId}_modal');
                $.ajax({
                    url: URL,
                    data: dataSet,
                    type: 'POST',
					dataType: "html"
                })
				.done(function(html) {
					//console.log("html ====> " + html);
					$(modal).html(html).foundation('reveal','open');
				});
            }
        </@treemenu_event>
    </@treemenu>
    <@modal id=productStoreId />
</#if>