
<#-- DEV NOTE: KEEP SETUP-SPECIFIC CONFIG OUT OF THIS FILE
    SO IT CAN BE FACTORED OUT LATER -->

<#if !ectTreeId?has_content>
  <#assign ectTreeId = "ectTree_" + rawString(productStoreId!)>
</#if>

<@script>
    function ScpCatalogTreeHandler(data) {
        var sctHandler = this;

        this.treeId = data.treeId;
        this.productStoreId = data.productStoreId;
        this.allActionProps = data.actionProps || {};
        var labels = data.labels || {}; 
        
        <#-- 
            Helpers
        -->
    
        var askConfirm = function(msg) {
            return confirm(msg); // TODO: modal-based
        };
        
        var reportError = function(msg) {
            alert(labels.error + ": " + msg);
        };
        var reportInternalError = function(msg) {
            alert("Internal error: " + msg);
        };

        var isUndefOrNull = function(obj) {
            return (typeof obj === 'undefined' || obj == null);
        };
        var isObj = function(obj) {
            return (jQuery.type(obj) === "object");
        };
        var isNonEmptyObj = function(obj) {
            return (jQuery.type(obj) === "object") && !jQuery.isEmptyObject(obj);
        };

        var appendLinkParams = function(url, params) {
            if (url) {
                if (isNonEmptyObj(params)) {
                    // FIXME?: JS-based param append not guaranteed to work all cases
                    // FIXME?: should try to replace existing params of same name for easier usage
                    url = url + ((url.indexOf('?') >= 0) ? '&' : '?') + $.param(params);
                }
            }
            return url;
        };
        var openLink = function(url, params, target) {
            if (url) {
                url = appendLinkParams(url, params);
                if (target) {
                    window.open(url, target);
                } else {
                    window.location = url;
                }
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
        var getParentNode = function($node) {
            <#-- TODO -->
            return null;
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
        this.getNodeObjectType = function($node) {
            return $node.data.type; // same names, 1-for-1 mapping
        };

        this.isActionPropsValid = function(objectType, actionName) {
            var actionGroup = this.allActionProps[objectType];
            if (!isNonEmptyObj(actionGroup)) return false;
            var action = actionGroup[actionName];
            if (!isNonEmptyObj(action)) return false;
            
            return action.type === "link" || action.type === "form";
        };
        this.getActionProps = function($node, objectType, actionName) {
            var actionGroup = this.allActionProps[objectType];
            if (!isNonEmptyObj(actionGroup)) return {"type":"none"};
            var action = actionGroup[actionName];
            if (!isNonEmptyObj(action)) return {"type":"none"};
        
            // COPY the object because will be modified
            return jQuery.extend(true, {}, action);
        };
        
        /* can avoid editing actionProps in-place for now
        this.setActionPropsParams = function(actionProps, params) {
            if (isNonEmptyObj(params)) {
                var currParams = actionProps.params;
                if (!isObj(currParams)) currParams = {};
                actionProps.params = jQuery.extend({}, currParams, params);
            }
        };
        */
        
        this.getResolvedActionPropsParams = function(actionProps, params) {
            var resParams = {};
            if (isUndefOrNull(params) {
                params = actionProps.params;
            }
            if (isNonEmptyObj(params)) {
                var explicitOnly = (actionProps.paramNamesMode === "explicit");
            
                // this is to support forms/links that need different field names or copied to multiple fields
                if (isObj(actionProps.paramNames)) {
                    jQuery.each(params, function(k, v) {
                        var replName = actionProps.paramNames[k];
                        if (!isUndefOrNull(replName)) {
                            if (jQuery.type(replName) === 'array') {
                                jQuery.each(replName, function(i, e) {
                                    resParams[e] = v;
                                });
                            } else {
                                if (jQuery.type(replName) !== 'boolean') { // boolean false prevents use
                                    resParams[replName] = v;
                                }
                            }
                        } else {
                            if (!explicitOnly) {
                                resParams[k] = v;
                            }
                        }
                    });
                } else {
                    if (!explicitOnly) {
                        resParams = jQuery.extend({}, params);
                    }
                }
            }
            return resParams;
        };
        
        this.populateForm = function(form, params) {
            if (isObj(params)) {
                jQuery.each(params, function(k, v) {
                    var input = jQuery('[name=' + k + ']', form).filter(':input');
                    if (isUndefOrNull(v)) {
                        v = '';
                    }
                    input.val(v);
                });
            }
        };
        
        // standard action target implementation (TODO?: callbacks or override into this)
        // NOTE: caller can pass params and call setActionPropsParams instead (convenience)
        this.execActionTarget = function(actionProps, params) {
            params = this.getResolvedActionPropsParams(actionProps, params);
            if (actionProps.type == "link") {
                openLink(actionProps.url, params, actionProps.target);
            } else if (actionProps.type == "form") {
                var form = jQuery('#' + actionProps.id);
                if (form.length) {
                    if (form.prop('tagName').toLowerCase() !== "form") {
                        form = jQuery('form', form); // find first child that is a form (convenience)
                    }
                    if (form.length) {
                        form = form.first();
                        if (actionProps.mode == "show") {
                            this.populateForm(form, params);
                            if (this.hideShowFormIds) {
                                jQuery.each(this.hideShowFormIds, function(i, e) {
                                    jQuery('#'+e).fadeOut();
                                });
                            }
                            jQuery('#'+actionProps.id).fadeOut();
                        } else (actionProps.mode == "submit") {
                            var doExec = true;
                            if (actionProps.confirmMsg) {
                                doExec = askConfirm(actionProps.confirmMsg);
                            }
                            if (doExec) {
                                this.populateForm(form, params);
                                form.submit();
                            }
                        }
                    } else {
                        reportInternalError("could not find form for container id: " + actionProps.id);
                    }
                } else {
                    reportInternalError("bad form id: " + actionProps.id);
                }
            }
        };

        <#-- 
            Core functions
        -->
        
        this.execEditForNode = function($node) {
            var objectType = this.getNodeObjectType($node);
            var actionProps = this.getActionProps($node, objectType, "manage");
            var id = getNodeOrigId($node);
            var data = $node.data;
            
            var params = {};
            if (objectType == "catalog") {
                params.prodCatalogId = id;
                params.productStoreId = this.productStoreId;
                // merge all, shouldn't be any conflicts...
                if (data.productStoreCatalogEntity) {
                    jQuery.extend(params, data.productStoreCatalogEntity);
                }
                if (data.prodCatalogEntity) {
                    jQuery.extend(params, data.prodCatalogEntity);
                }
            } else if (objectType == "category") {
                params.prodCatalogId = getNodeOrigId(getTopNode($node));
                params.productCategoryId = id;
                params.productStoreId = this.productStoreId;
                if (data.productCategoryRollupEntity) {
                    jQuery.extend(params, data.productCategoryRollupEntity);
                }
                if (data.prodCatalogCategoryEntity) {
                    jQuery.extend(params, data.prodCatalogCategoryEntity);
                }
                if (data.productCategoryEntity) {
                    jQuery.extend(params, data.productCategoryEntity);
                }
            }
            
            this.execActionTarget(actionProps, params);
        };
        
        this.execRemoveForNode = function($node) {
            var objectType = this.getNodeObjectType($node);
            var actionProps = this.getActionProps($node, objectType, "remove");
            var id = getNodeOrigId($node);
            
            var params = {};
            if (objectType == "catalog") {
                params.productStoreId = this.productStoreId;
                params.prodCatalogId = id;
            } else if (objectType == "category") {
                params.productStoreId = this.productStoreId;
                params.productCategoryId = id;
                var parentNode = this.getParentNode($node);
                var parentType = this.getNodeObjectType(parentNode);
                if (parentType == "catalog") {
                    params.prodCatalogId = getNodeOrigId(parentNode);
                } else if (parentType == "category") {
                    params.parentProductCategoryId = getNodeOrigId(parentNode);
                } else {
                    reportInternalError("category node's parent is not catalog or category: " + id);
                }
            }
            
            this.execActionTarget(actionProps, params);
        };
        
        this.execNewCategoryForNode = function($node) {
            var objectType = this.getNodeObjectType($node);
            var actionProps = this.getActionProps($node, objectType, "newcategory");
            var id = getNodeOrigId($node);
            
            var params = {};
            if (objectType == "catalog") {
                params.productStoreId = this.productStoreId;
                params.prodCatalogId = id;
            } else if (objectType == "category") {
                params.productStoreId = this.productStoreId;
                params.productCategoryId = id;
                var parentNode = this.getParentNode($node);
                var parentType = this.getNodeObjectType(parentNode);
                if (parentType == "catalog") {
                    params.prodCatalogId = getNodeOrigId(parentNode);
                } else if (parentType == "category") {
                    params.parentProductCategoryId = getNodeOrigId(parentNode);
                } else {
                    reportInternalError("category node's parent is not catalog or category: " + id);
                }
            }
            
            this.execActionTarget(actionProps, params);
        };
        
        this.execManageForNode = function($node) {
            var objectType = this.getNodeObjectType($node);
            var actionProps = this.getActionProps($node, objectType, "manage");
            var id = getNodeOrigId($node);
            
            var params = {};
            if (objectType == "catalog") {
                params.prodCatalogId = id;
                params.productStoreId = this.productStoreId;
            } else if (objectType == "category") {
                params.productCategoryId = id;
                params.productStoreId = this.productStoreId;
            } else if (objectType == "product") {
                params.productId = id;
                params.productStoreId = this.productStoreId;
            }
            
            this.execActionTarget(actionProps, params);
        };
        
        <#-- 
            Menu plugs
        -->

        this.getMenuActionDefs = function($node) {
            return {
                edit: {
                    "separator_before": false,
                    "separator_after": false,
                    "label": labels.edit,
                    "action": function(obj) { <#-- FIXME?: should get node from obj, not $node? -->
                        sctHandler.execEditForNode($node);
                    }
                },
                remove: {
                    "separator_before": false,
                    "separator_after": false,
                    "label": labels.remove,
                    "action": function(obj) {
                        sctHandler.execRemoveForNode($node);
                    }
                },
                newcategory: {
                    "separator_before": false,
                    "separator_after": false,
                    "label": labels.newcategory,
                    "action": function(obj) {
                        sctHandler.execNewCategoryForNode($node);
                    }
                },
                manage: {
                    "separator_before": true,
                    "separator_after": false,
                    "label": labels.manage,
                    "action": function(obj) {
                        sctHandler.execManageForNode($node);
                    }
                }
            };
        };
        
        this.getMenuDefs = function($node) {
            var defs = this.getMenuActionDefs($node);
            var objectType = this.getNodeObjectType($node);
            
            var menuDefs = {};
            if (objectType == 'catalog') {
                menuDefs = {
                    "edit": defs.edit,
                    "remove": defs.remove,
                    "newcategory": defs.newcategory,
                    "manage": defs.manage
                };
            } else if (objectType == 'category') {
                menuDefs = {
                    "edit": defs.edit,
                    "remove": defs.remove,
                    "newcategory": defs.newcategory,
                    "manage": defs.manage
                };
            } else if (objectType == 'product') {
                menuDefs = {
                    "manage": defs.manage
                };
            }
            
            // filter - allows caller to omit certain menu items
            var resMenuDefs = {};
            jQuery.each(menuDefs, function(k, v) {
                if (sctHandler.isActionPropsValid(objectType, k) {
                    resMenuDefs[k] = v;
                }
            });
            return resMenuDefs;
        };

        this.dropMenuHandler = function($node) {
            return this.getMenuDefs($node);
        };
        
        this.sideMenuHandler = function($node) {
            var $el = $("#ect-action-menu");
            var newOptions = this.getMenuDefs($node);

            $el.empty(); // remove old options
            $.each(newOptions, function(key, actionDef) {
                var newEl = $(sctHandler.emptyMenuMarkup || '<ul></ul>');
                var menuAnchor = $(newEl).find('a:last-child');
                menuAnchor.attr("href","javascript:void(0);").text(actionDef.label);
                menuAnchor.click(actionDef.action);
                $el.append(newEl);
            });
        };
    }
    
    var etcHandler = new ScpCatalogTreeHandler({
        treeId: "${escapeVal(ectTreeId!, 'js')}",
        productStoreId: "${escapeVal(productStoreId!, 'js')}",
        actionProps: <@objectAsScript object=(etcActionProps!{}) lang='js'/>,
        hideShowFormIds: <@objectAsScript object=(etcAllHideShowFormIds![]) lang='js'/>,
        labels: {
            error: "${escapeVal(uiLabelMap.CommonError, 'js')}",
            edit: "${escapeVal(uiLabelMap.CommonEdit, 'js')}",
            remove: "${escapeVal(uiLabelMap.CommonRemove, 'js')}",
            newcategory: "${escapeVal(uiLabelMap.ProductNewCategory, 'js')}",
            manage: "${escapeVal(uiLabelMap.CommonManage, 'js')}"
        },
        emptyMenuMarkup: '<@compress_single_line><@menuitem type="link" href="" text=""/></@compress_single_line>';
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
      
      <#if ectExtrasArea??>
        <#if ectExtrasArea?is_directive>
          <@ectExtrasArea/>
        <#elseif isObjectType("string", ectExtrasArea)>
          ${ectExtrasArea}
        </#if>
      </#if>
    </@cell>
</@row>


