<#if security.hasEntityPermission("CATALOG", "_VIEW", session)>
    <#assign treePlugin = [{"name":"massload"}]/>
    <#assign menuEvent>
            var $node = data.node;
            id = $node.id;
            var dataSet;
            if ($node.data.li_attr.original_id && $node.data.li_attr.original_id != id)
                id = $node.data.li_attr.original_id;
            if($node.data.type == "catalog") {
                URL = 'EditProdCatalog';
                dataSet = {"prodCatalogId" : id};
            } else if($node.data.type == "category") {
                URL = 'EditCategory';
                dataSet = {"productCategoryId" : id};
            } else if($node.data.type == "product") {
                URL = 'ViewProduct';
                dataSet = {"productId" : id};
            }
            if (URL && dataSet) {
                window.location = URL + '?' + $.param(dataSet);  
            }
        </#assign>
    <@treemenu  id=productStoreId settings=treeMenuSettings plugins=treePlugin events={"activate_node.jstree":menuEvent}>
        <#list treeMenuData as node>
            <#assign nState = node.state.opened/>
            <#switch node.type>
                <#case "product">
                    <@treeitem text=node.text!"" id=node.id!  parent=(node.parent!"#") 
                    attribs={"data":{"type":"${node.type!}","li_attr":node.li_attr}} 
                    state=nState
                    icon="${styles.text_color_secondary!} ${styles.icon!} ${styles.icon_prefix!}file"/>
                <#break>
                <#case "category">
                    <#if nState>
                        <@treeitem text=node.text!"" id=node.id!  parent=(node.parent!"#") 
                        attribs={"data":{"type":"${node.type!}","li_attr":node.li_attr}} 
                        state=node.state
                        icon="${styles.text_color_secondary!} ${styles.icon!} ${styles.icon_prefix!}folder-open"/>
                    <#else>
                        <@treeitem text=node.text!"" id=node.id!  parent=(node.parent!"#") 
                        attribs={"data":{"type":"${node.type!}","li_attr":node.li_attr}} 
                        state=nState
                        icon="${styles.text_color_secondary!} ${styles.icon!} ${styles.icon_prefix!}folder"/>
                    </#if>
                <#break>
                <#case "catalog">
                    <@treeitem text=node.text!"" id=node.id!  parent=(node.parent!"#") 
                    attribs={"data":{"type":"${node.type!}","li_attr":node.li_attr}} 
                    state=nState
                    icon="${styles.text_color_secondary!} ${styles.icon!} ${styles.icon_prefix!}cubes"/>
                <#break>
            </#switch>
        </#list>
    </@treemenu>
    <@modal id=productStoreId />
</#if>
