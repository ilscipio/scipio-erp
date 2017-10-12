
<#assign treeId = "setupEditCatalogTree_" + rawString(productStoreId!)>

<@script>
    function ScpCatalogTreeHandler(data) {
        this.productStoreId = data.productStoreId;
        this.linkMap = data.linkMap;

        <#-- 
            Helpers
        -->

        var appendLinkParams = function(url, params) {
            if (url) {
                if (params && !jQuery.isEmptyObject(params)) {
                    <#-- FIXME?: JS-based param append not guaranteed to work all cases -->
                    url = url + ((url.indexOf('?') >= 0) ? '&' : '?') + $.param(params);
                }
            }
            return url;
        };

        var openLink = function(url, params) {
            if (url) {
                window.location = appendLinkParams(url, params);
            }
        };
        
        var openExtLink = function(url, params) {
            if (url) {
                window.open(appendLinkParams(url, params), '_blank');
            }
        };

        var getNodeOrigId = function($node) {
            var id = $node.id;
            if ($node.data.li_attr.original_id && $node.data.li_attr.original_id != id) {
                id = $node.data.li_attr.original_id;
            }
            return id;
        };
        
        var getParentNodeOrigId = function($node) {
            <#-- TODO -->
        };


        <#-- 
            Core functions
        -->

        this.makeManageLinkForNode = function($node) {
            var id = getNodeOrigId($node);
            var url = null;
            var params = {};
            
            if ($node.data.type == "catalog") {
                url = this.linkMap.manageCatalog;
                params.prodCatalogId = id;
                params.productStoreId = this.productStoreId;
            } else if ($node.data.type == "category") {
                url = this.linkMap.manageCategory;
                params.productCategoryId = id;
                params.productStoreId = this.productStoreId;
            } else if ($node.data.type == "product") {
                url = this.linkMap.manageProduct;
                params.productId = id;
                params.productStoreId = this.productStoreId;
            }
        
            if (url) {
                return appendLinkParams(url, params);
            }
            return null;
        };
        
        this.execManageForNode = function($node) {
            openExtLink(this.makeManageLinkForNode($node));
        };
        
        this.makeEditLinkForNode = function($node) {
            return null;
        };
        
        this.execEditForNode = function($node) {
            alert("TODO");
            openLink(this.makeEditLinkForNode($node));
        };
        
        this.makeRemoveLinkForNode = function($node) {
            return null;
        };
        
        this.execRemoveForNode = function($node) {
            // TODO: CONFIRM
            // TODO: needs POST form
            alert("TODO");
            //openLink(this.makeRemoveLinkForNode($node));
        };
        
        this.makeNewCategoryLinkForNode = function($node) {
            // parentProductCategoryId
        
            return null;
        };
        
        this.execNewCategoryForNode = function($node) {
            alert("TODO");
            openLink(this.makeNewCategoryLinkForNode($node));
        };
        
        
        <#-- 
            Menu plugs
        -->

        this.dropMenuHandler = function($node) {
            var sctHandler = this;
            
            var editDef = {
                "separator_before": false,
                "separator_after": false,
                "label": "${escapeVal(uiLabelMap.CommonEdit, 'js')}",
                "action": function(obj) { <#-- FIXME?: should get node from obj, not $node? -->
                    sctHandler.execEditForNode($node);
                }
            };
            var removeDef = {
                "separator_before": false,
                "separator_after": false,
                "label": "${escapeVal(uiLabelMap.CommonRemove, 'js')}",
                "action": function(obj) {
                    sctHandler.execRemoveForNode($node);
                }
            };
            var newCategoryDef = {
                "separator_before": false,
                "separator_after": false,
                "label": "${escapeVal(uiLabelMap.ProductNewCategory, 'js')}",
                "action": function(obj) {
                    sctHandler.execNewCategoryForNode($node);
                }
            };
            var manageDef = {
                "separator_before": true,
                "separator_after": false,
                "label": "${escapeVal(uiLabelMap.CommonManage, 'js')}",
                "action": function(obj) {
                    sctHandler.execManageForNode($node);
                }
            };

            if ($node.data.type == 'catalog') {
                return {
                    "Edit": editDef,
                    "Remove": removeDef,
                    "NewCategory": newCategoryDef,
                    "Manage": manageDef
                };
            } else if ($node.data.type == 'category') {
                return {
                    "Edit": editDef,
                    "Remove": removeDef,
                    "NewCategory": newCategoryDef,
                    "Manage": manageDef
                };
            } else if ($node.data.type == 'product') {
                return {
                    "Manage": manageDef
                };
            } else {
                return {};
            }
        };
        
        this.separateMenuHandler = function($node) {
            var sctHandler = this;
            <#-- TODO?: if right-click menu is not enough
            var $el = $("#action_menu");

            var newOptions;
            if ($node.data.type == 'catalog') {
                newOptions = {
                  "${escapeVal(uiLabelMap.CommonCreate, 'js')}": "TODO"
                }; 
            } else if ($node.data.type == 'category') {
                newOptions = {
                  "${escapeVal(uiLabelMap.CmsOverride, 'js')}": "TODO"
                };
            } else if ($node.data.type == 'product') {
                newOptions = {
                  "${escapeVal(uiLabelMap.CommonCreate, 'js')}": "TODO",
                  "${escapeVal(uiLabelMap.CommonOpen, 'js')}": "TODO"
                };
            }
            $el.empty(); // remove old options
            $.each(newOptions, function(key,value) {
                var newEl = $('<@compress_single_line><@menuitem type="link" href="" text=""/></@compress_single_line>');
                var menuAnchor = $(newEl).find('a:last-child');
                menuAnchor.attr("href",value).text(key);
                $el.append(newEl);
            });
            -->
        };
    }
    
    var setcHandler = new ScpCatalogTreeHandler({
        productStoreId: "${escapeVal(productStoreId!, 'js')}",
        linkMap: {
            manageCatalog: '<@ofbizInterWebappUrl uri='/catalog/control/EditProdCatalog' escapeAs='js' extLoginKey=true/>',
            manageCategory: '<@ofbizInterWebappUrl uri='/catalog/control/EditCategory' escapeAs='js' extLoginKey=true/>',
            manageProduct: '<@ofbizInterWebappUrl uri='/catalog/control/ViewProduct' escapeAs='js' extLoginKey=true/>',
        }
    });
    
</@script>
<#assign treeEvents = {
    'select_node.jstree': 'setcHandler.separateMenuHandler(data.node);'
    <#--'activate_node.jstree': 'setcHandler.execManageForNode(data.node);',-->
    <#-- no longer supported by jstree
    'dblclick.jstree': 'setcHandler.execManageForNode(data.node);'-->
}/>

<#assign contextMenuPluginSettings = {
    "items": wrapRawScript("function(node) { return setcHandler.dropMenuHandler(node); }")
}/>
<#assign treePlugins = [
    {"name":"contextmenu", "settings":contextMenuPluginSettings},
    {"name":"massload"}
]/>

<@treemenu id=treeId settings=treeMenuSettings plugins=treePlugins events=treeEvents>
    <#list treeMenuData as node>
        <#switch rawString(node.type)>
            <#case "product">
                <@treeitem text=(node.text!"") id=(node.id!) parent=(node.parent!"#") 
                    attribs={"data":{"type":"${node.type!}","li_attr":node.li_attr}} 
                    state=(node.state!{})
                    icon="${styles.text_color_secondary!} ${styles.icon!} ${styles.icon_prefix!}file"/>
            <#break>
            <#case "category">
                <#if node.state.opened>
                    <@treeitem text=(node.text!"") id=(node.id!) parent=(node.parent!"#") 
                        attribs={"data":{"type":"${node.type!}","li_attr":node.li_attr}} 
                        state=(node.state!{})
                        icon="${styles.text_color_secondary!} ${styles.icon!} ${styles.icon_prefix!}folder-open"/>
                <#else>
                    <@treeitem text=(node.text!"") id=(node.id!) parent=(node.parent!"#") 
                        attribs={"data":{"type":"${node.type!}","li_attr":node.li_attr}} 
                        state=(node.state!{})
                        icon="${styles.text_color_secondary!} ${styles.icon!} ${styles.icon_prefix!}folder"/>
                </#if>
            <#break>
            <#case "catalog">
                <@treeitem text=(node.text!"") id=(node.id!) parent=(node.parent!"#") 
                    attribs={"data":{"type":"${node.type!}","li_attr":node.li_attr}} 
                    state=(node.state!{})
                    icon="${styles.text_color_secondary!} ${styles.icon!} ${styles.icon_prefix!}cubes"/>
            <#break>
        </#switch>
    </#list>
</@treemenu>
<#--<@modal id=treeId />-->


<div class="sect-newcategory-area" style="display:none;">
  <@section title=uiLabelMap.PartyNewCategory>
    <@form>
        <@field type="hidden" name="parentProductCategoryId" value=""/>
        <@field type="hidden" name="prodCatalogId" value=""/>
    </@form>
  </@section>
</div>

