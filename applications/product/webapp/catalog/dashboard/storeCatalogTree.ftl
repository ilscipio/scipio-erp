<#if security.hasEntityPermission("CATALOG", "_VIEW", session)>    
    <@treemenu id=productStoreId data=treeMenuData settings=treeMenuSettings>
        <@treemenu_event event="changed.jstree">
            console.log("selected node =====> " + data.selected[0]);
            /* $.ajax({
                url: 'EditProdCatalogAjax',
                type: 'POST',
                error: function(msg) {
                    alert('An error occurred loading content! : ' + msg);
                },
                success: function(msg) {
                    
                    $('#${productStoreId}').html(msg);
                }
            }); */
        </@treemenu_event>
    </@treemenu>



</#if>