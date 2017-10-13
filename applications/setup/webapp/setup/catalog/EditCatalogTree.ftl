<#include "component://setup/webapp/setup/common/common.ftl">

<#-- TODO: REVIEW: may need get/post forms instead of URLs like this, param issues -->
<#assign etcLinkMap = {
    "manageCatalog": makeOfbizInterWebappUrl({"uri":'/catalog/control/EditProdCatalog', "extLoginKey":true}),
    "manageCategory": makeOfbizInterWebappUrl({"uri":'/catalog/control/EditCategory', "extLoginKey":true}),
    "manageProduct": makeOfbizInterWebappUrl({"uri":'/catalog/control/ViewProduct', "extLoginKey":true}),
    
    <#--"newCatalog": "",-->
    "newCategory": "",
    
    "editCatalog": makeSetupStepUrl("catalog", true, ["prodCatalogId"], "&"),
    "editCategory": makeSetupStepUrl("catalog", true, ["productCategoryId"], "&"),
    
    "removeCatalog": "",
    "removeCategory": ""
}>

<#-- DEV NOTE: KEEP SETUP-SPECIFIC CONFIG VARIABLES ABOVE THIS COMMENT
    SO THE BELOW CAN BE FACTORED OUT LATER -->

<#if !ectTreeId?has_content>
  <#assign ectTreeId = "editCatalogTree_" + rawString(productStoreId!)>
</#if>

<@script>
    function ScpCatalogTreeHandler(data) {
        this.treeId = data.treeId;
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
            if (!$node) return null;
            var id = $node.id;
            if ($node.data.li_attr.original_id && $node.data.li_attr.original_id != id) {
                id = $node.data.li_attr.original_id;
            }
            return id;
        };
        
        var getParentNodeOrigId = function($node) {
            <#-- TODO -->
            return null;
        };

        var getTopNode = function($node) {
            <#-- TODO -->
            return null;
        };

        this.getJsTree = function() {
            return jQuery('#'+this.treeId).jstree(true);
        };

        this.getNodeById = function(id) {
            return this.getJsTree().get_node(id);
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
            var id = getNodeOrigId($node);
            var url = null;
            var params = {};
            
            if ($node.data.type == "catalog") {
                url = this.linkMap.editCatalog;
                params.prodCatalogId = id;
                params.productStoreId = this.productStoreId;
            } else if ($node.data.type == "category") {
                url = this.linkMap.editCategory;
                params.prodCatalogId = getNodeOrigId(getTopNode($node));
                params.productCategoryId = id;
                params.productStoreId = this.productStoreId;
            }
        
            if (url) {
                return appendLinkParams(url, params);
            }
            return null;
        };
        
        this.execEditForNode = function($node) {
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

        this.getActionDefs = function($node) {
            var sctHandler = this;
            return {
                edit: {
                    "separator_before": false,
                    "separator_after": false,
                    "label": "${escapeVal(uiLabelMap.CommonEdit, 'js')}",
                    "action": function(obj) { <#-- FIXME?: should get node from obj, not $node? -->
                        sctHandler.execEditForNode($node);
                    }
                },
                remove: {
                    "separator_before": false,
                    "separator_after": false,
                    "label": "${escapeVal(uiLabelMap.CommonRemove, 'js')}",
                    "action": function(obj) {
                        sctHandler.execRemoveForNode($node);
                    }
                },
                newCategory: {
                    "separator_before": false,
                    "separator_after": false,
                    "label": "${escapeVal(uiLabelMap.ProductNewCategory, 'js')}",
                    "action": function(obj) {
                        sctHandler.execNewCategoryForNode($node);
                    }
                },
                manage: {
                    "separator_before": true,
                    "separator_after": false,
                    "label": "${escapeVal(uiLabelMap.CommonManage, 'js')}",
                    "action": function(obj) {
                        sctHandler.execManageForNode($node);
                    }
                }
            };
        };

        this.getMenuDefs = function($node) {
            var actionDefs = this.getActionDefs($node);
            if ($node.data.type == 'catalog') {
                return {
                    "Edit": actionDefs.edit,
                    "Remove": actionDefs.remove,
                    "NewCategory": actionDefs.newCategory,
                    "Manage": actionDefs.manage
                };
            } else if ($node.data.type == 'category') {
                return {
                    "Edit": actionDefs.edit,
                    "Remove": actionDefs.remove,
                    "NewCategory": actionDefs.newCategory,
                    "Manage": actionDefs.manage
                };
            } else if ($node.data.type == 'product') {
                return {
                    "Manage": actionDefs.manage
                };
            } else {
                return {};
            }
        };

        this.dropMenuHandler = function($node) {
            return this.getMenuDefs($node);
        };
        
        this.sideMenuHandler = function($node) {
            var $el = $("#ect-action-menu");
            var newOptions = this.getMenuDefs($node);

            $el.empty(); // remove old options
            $.each(newOptions, function(key, actionDef) {
                var newEl = $('<@compress_single_line><@menuitem type="link" href="" text=""/></@compress_single_line>');
                var menuAnchor = $(newEl).find('a:last-child');
                menuAnchor.attr("href","javascript:void(0);").text(actionDef.label);
                menuAnchor.click(actionDef.action);
                $el.append(newEl);
            });
        };
    }
    
    var etcHandler = new ScpCatalogTreeHandler({
        treeId: "${escapeVal(ectTreeId, 'js')}",
        productStoreId: "${escapeVal(productStoreId!, 'js')}",
        linkMap: <@objectAsScript object=etcLinkMap lang='js'/>
    });
    
</@script>
<#assign treeEvents = {
    'select_node.jstree': 'etcHandler.sideMenuHandler(data.node);'
    <#--'activate_node.jstree': 'etcHandler.execManageForNode(data.node);',-->
    <#-- no longer supported by jstree
    'dblclick.jstree': 'etcHandler.execManageForNode(data.node);'-->
}/>

<#assign contextMenuPluginSettings = {
    "items": wrapRawScript("function(node) { return etcHandler.dropMenuHandler(node); }")
}/>
<#assign treePlugins = [
    {"name":"contextmenu", "settings":contextMenuPluginSettings},
    {"name":"massload"}
]/>

<@row>
    <@cell medium=9 large=9>
      <@section title=uiLabelMap.ProductBrowseCatalogeAndCategories>
        <@treemenu id=ectTreeId settings=treeMenuSettings plugins=treePlugins events=treeEvents>
            <#list treeMenuData as node>
                <#switch rawString(node.type)>
                    <#case "product">
                        <@treeitem text=(node.text!"") id=(node.id!) parent=(node.parent!"#") 
                            attribs={"data":{"type":"${node.type!}","li_attr":node.li_attr}} 
                            state=(node.state!{})
                            icon="${styles.text_color_secondary!} ${styles.icon!} ${styles.icon_prefix!}file"/>
                    <#break>
                    <#case "category">
                        <#assign text = rawString(node.text!"")>
                        <#assign sequenceNum = (node.productCategoryRollupEntity.sequenceNum)!(node.prodCatalogCategoryEntity.sequenceNum)!false>
                        <#if sequenceNum?is_number>
                          <#assign text = text + " #" + sequenceNum>
                        </#if>
                        <@treeitem text=text id=(node.id!) parent=(node.parent!"#") 
                            attribs={"data":{
                                "type":"${node.type!}",
                                "li_attr":node.li_attr,
                                <#-- TODO: OPTIMIZE: print out only needed fields -->
                                "productCategoryRollupEntity":node.productCategoryRollupEntity!{},
                                "prodCatalogCategoryEntity":node.prodCatalogCategoryEntity!{},
                                "productCategoryEntity":node.productCategoryEntity!{}
                            }} 
                            state=(node.state!{})
                            icon=("${styles.text_color_secondary!} ${styles.icon!} ${styles.icon_prefix!}"+(node.state.opened?string("folder-open","folder")))/>
                    <#break>
                    <#case "catalog">
                        <#assign text = rawString(node.text!"")>
                        <#assign sequenceNum = (node.productStoreCatalogEntity.sequenceNum)!false>
                        <#if sequenceNum?is_number>
                          <#assign text = text + " #" + sequenceNum>
                        </#if>
                        <@treeitem text=text id=(node.id!) parent=(node.parent!"#") 
                            attribs={"data":{
                                "type":"${node.type!}",
                                "li_attr":node.li_attr,
                                <#-- TODO: OPTIMIZE: print out only needed fields -->
                                "productStoreCatalogEntity":node.productStoreCatalogEntity!{},
                                "prodCatalogEntity":node.prodCatalogEntity!{}
                            }} 
                            state=(node.state!{})
                            icon="${styles.text_color_secondary!} ${styles.icon!} ${styles.icon_prefix!}cubes"/>
                    <#break>
                </#switch>
            </#list>
        </@treemenu>
      </@section>
    </@cell>
    <@cell medium=3 large=3>
      <@section title=uiLabelMap.CommonActions id="ect-action-menu-offset">
        <ul class="side-nav" id="ect-action-menu">
        </ul>
      </@section>
    </@cell>
</@row>

<@section title=uiLabelMap.PartyNewCatalog containerClass="+ect-newcatalog" containerStyle="display:none;">
  <@form id="ect-newcatalog-form">
    <@field type="hidden" name="prodCatalogId" value=""/>
  </@form>
</@section>

<@section title=uiLabelMap.PartyEditCatalog containerClass="+ect-editcatalog" containerStyle="display:none;">
  <@form id="ect-editcatalog-form">
    <@field type="hidden" name="prodCatalogId" value=""/>
  </@form>
</@section>

<@section title=uiLabelMap.PartyNewCategory containerClass="+ect-newcategory" containerStyle="display:none;">
  <@form id="ect-newcategory-form">
      <@field type="hidden" name="parentProductCategoryId" value=""/>
      <@field type="hidden" name="prodCatalogId" value=""/>
  </@form>
</@section>

<@section title=uiLabelMap.PartyEditCategory containerClass="+ect-editcategory" containerStyle="display:none;">
  <@form id="ect-editcategory-form">
      <@field type="hidden" name="parentProductCategoryId" value=""/>
      <@field type="hidden" name="prodCatalogId" value=""/>
  </@form>
</@section>

