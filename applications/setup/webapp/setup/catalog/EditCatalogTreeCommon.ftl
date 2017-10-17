
<#-- DEV NOTE: KEEP SETUP-SPECIFIC CONFIG OUT OF THIS FILE
    SO IT CAN BE FACTORED OUT LATER (TODO) -->

<#if !ectTreeId?has_content>
  <#assign ectTreeId = "ectTree_" + rawString(productStoreId!)>
</#if>

<#-- FIXME: private vs public methods are kind of arbitrary for now until code settles -->
<@script>
    function ScpCatalogTreeHandler(data) { // TODO?: this object could go in js file
        var scth = this; // capture for private methods and js kludges

        scth.fadeOptions = data.fadeOptions || {};
        scth.treeId = data.treeId;
        scth.productStoreId = data.productStoreId;
        scth.allActionProps = data.actionProps || {};
        scth.emptyMenuMarkup = data.emptyMenuMarkup;
        scth.hideShowFormIds = data.hideShowFormIds;
        scth.labels = data.labels || {}; 

        <#-- 
            Helpers
        -->
    
        var askConfirm = function(msg) {
            return confirm(msg); // TODO: modal-based
        };
        
        var reportError = function(msg) {
            alert(scth.labels.error + ": " + msg);
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
        var isSpecParamVal = function(val) {
            // SPECIAL: we may store sub-objects in the params map to pass around; this detects them
            return isObj(val);
        };

        var appendLinkParams = function(url, params) {
            if (url) {
                if (isNonEmptyObj(params)) {
                    var effParams = {};
                    jQuery.each(params, function(k, v) {
                        if (!isSpecParamVal(v)) {
                            effParams[k] = v;
                        }
                    });
                    if (isNonEmptyObj(effParams)) {
                        // FIXME?: JS-based param append not guaranteed to work all cases
                        // FIXME?: should try to replace existing params of same name for easier usage
                        url = url + ((url.indexOf('?') >= 0) ? '&' : '?') + $.param(effParams);
                    }
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

        var getJsTree = function() {
            var jsTree = scth.jsTree;
            if (!jsTree) {
                jsTree = jQuery('#'+scth.treeId).jstree(true); // true = get without creating
                scth.jsTree = jsTree
            }
            return jsTree;
        };
        this.getJsTree = function() { // public
            return getJsTree();
        };
        var getNodeById = function(id) {
            return scth.getJsTree().get_node(id);
        };
        this.getNodeById = function(id) { // public
            return getNodeById(id);
        };
        var checkGetNodeById = function(nodeOrId) {
            if (jQuery.type(nodeOrId) === 'string') {
                return getNodeById(nodeOrId);
            } else {
                return nodeOrId;
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
        var getNodeObjectId = function($node) {
            return getNodeOrigId($node);
        };
        var getParentNode = function($node) {
            if ($node) {
                var jsTree = getJsTree();
                // DEV NOTE: you could do (parentId = $node.parent), but this looks more future-proof
                var parentId = jsTree.get_parent($node);
                if (parentId) {
                    return jsTree.get_node(parentId);
                }
            }
            return null;
        };
        var isRootNode = function($node) { // true if the node is the root, above the catalogs
            return ($node.id === "#"); // FIXME?: jstree.root is not documented, using this for now...
        }
        var getTopLevelNode = function($node) {
            var $parent = getParentNode($node);
            while($parent && !isRootNode($parent)) {
                $node = $parent;
                $parent = getParentNode($node);
            }
            return ($parent) ? $node : null; // prevents returning the root node
        };
        var getCatalogNodeForNode = function($node) {
            return getTopLevelNode($node);
        }
        var getNodeObjectType = function($node) {
            return $node.data.type; // same names, 1-for-1 mapping
        };

        var isActionPropsValid = function(objectType, actionName) {
            var actionGroup = scth.allActionProps[objectType];
            if (!isNonEmptyObj(actionGroup)) return false;
            var action = actionGroup[actionName];
            if (!isNonEmptyObj(action)) return false;
            
            return action.type === "link" || action.type === "form";
        };
        var getActionProps = function($node, objectType, actionName) {
            var actionGroup = scth.allActionProps[objectType];
            if (!isNonEmptyObj(actionGroup)) return {"type":"none"};
            var action = actionGroup[actionName];
            if (!isNonEmptyObj(action)) return {"type":"none"};
        
            // COPY the object because will be modified
            return jQuery.extend(true, {}, action);
        };
        
        /* can avoid editing actionProps in-place for now
        var setActionPropsParams = function(actionProps, params) {
            if (isNonEmptyObj(params)) {
                var currParams = actionProps.params;
                if (!isObj(currParams)) currParams = {};
                actionProps.params = jQuery.extend({}, currParams, params);
            }
        };
        */
        
        var getResolvedActionPropsParams = function(actionProps, params) {
            var resParams = {};
            if (isUndefOrNull(params)) {
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
        
        /**
         * DOES NOTHING by default - cannot clear like this, 
         * because destroys important hidden fields.
         */
        this.clearFormCommon = function(form, params, actionProps, $node) {
            //jQuery(':input', form).val('');
            //jQuery('.ect-displayfield', form).html('');
            //jQuery('.ect-managefield', form).html('');
        };
        
        this.makeManageLinkForElem = function(manageField, k, v, form, params, actionProps, $node) {
            if (manageField && manageField.length) {
                // FIXME: unhardcode markup
                var markup = jQuery('<a href="javascript:void(0);" class="ect-managefield-link">' + v + '</a>');
                markup.click(function() {
                    scth.execManageForNode($node);
                });
                manageField.html(markup);
            }
        };
        
        this.populateFormFieldCommon = function(k, v, form, params, actionProps, $node) {
            if (isUndefOrNull(v)) {
                v = '';
            }
            var input = jQuery('[name=' + k + ']', form).filter(':input');
            input.val(v);
            
            var dispField = jQuery('.ect-displayfield-for-' + k, form);
            if (dispField.length) {
                dispField.html(v);
            }
            
            var manageField = jQuery('.ect-managefield-for-' + k, form);
            if (manageField.length) {
                scth.makeManageLinkForElem(manageField, k, v, form, params, actionProps, $node);
            }
        };
        
        /**
         * default populate form implementation.
         * each param entry goes into "[name=xxxx]:input", ".ect-displayfield-for-xxx",
         * or ".ect-managefield-for-xxx".
         */
        this.populateFormCommon = function(form, params, actionProps, $node) {
            if (isObj(params)) {
                var fieldHandlers = actionProps.populateFormFields || {};
                jQuery.each(params, function(k, v) {
                    if (!isSpecParamVal(v)) {
                        var execCommon = true;
                        if (fieldHandlers[k]) {
                            execCommon = fieldHandlers[k](k, v, form, params, actionProps, $node);
                            if (execCommon !== false) {
                                execCommon = true;
                            }
                        }
                        if (execCommon) {
                            scth.populateFormFieldCommon(k, v, form, params, actionProps, $node);
                        }
                    }
                });
            }
        };
        
        var populateForm = function(form, params, actionProps, $node) {
            if (actionProps.clearForm !== false) {
                var execClearCommon = true; 
                if (jQuery.type(actionProps.clearForm) === 'function') {
                    execClearCommon = actionProps.clearForm(form, params, actionProps, $node, scth);
                    if (execClearCommon !== false) {
                        execClearCommon = true;
                    }
                }
                if (execClearCommon) {
                    scth.clearFormCommon(form, params, actionProps, $node);
                }
            }
            var execCommon = true; 
            if (jQuery.type(actionProps.populateForm) === 'function') {
                execCommon = actionProps.populateForm(form, params, actionProps, $node, scth);
                if (execCommon !== false) {
                    execCommon = true;
                }
            } 
            if (execCommon) {
                scth.populateFormCommon(form, params, actionProps, $node);
            }
        };
        
        // standard action target implementation (TODO?: callbacks or override into this)
        // NOTE: caller can pass params and call setActionPropsParams instead (convenience)
        var execActionTarget = function($node, actionProps, params) {
            params = getResolvedActionPropsParams(actionProps, params);
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
                            populateForm(form, params, actionProps, $node);
                            if (scth.hideShowFormIds) {
                                jQuery.each(scth.hideShowFormIds, function(i, e) {
                                    jQuery('#'+e).fadeOut(scth.fadeOptions);
                                });
                            }
                            jQuery('#'+actionProps.id).fadeIn(scth.fadeOptions);
                        } else if (actionProps.mode == "submit") {
                            var doExec = true;
                            if (actionProps.confirmMsg) {
                                doExec = askConfirm(actionProps.confirmMsg);
                            }
                            if (doExec) {
                                populateForm(form, params, actionProps, $node);
                                form.submit();
                            }
                        } else {
                            reportInternalError("invalid form action mode: " + actionProps.mode);
                        }
                    } else {
                        reportInternalError("could not find form for container id: " + actionProps.id);
                    }
                } else {
                    reportInternalError("bad form id: " + actionProps.id);
                }
            } else if (actionProps.type && actionProps.type != "none") {
                reportInternalError("invalid action type: " + actionProps.type);
            }
        };

        <#-- 
            Core functions
        -->
        
        this.execEditForNode = function($node) {
            $node = checkGetNodeById($node);
            var objectType = getNodeObjectType($node);
            
            /* DEV NOTE: not doing this for now, don't like the result, confusing
            // SPECIAL: for product, we'll make edit action trigger edit on parent category instead
            // TODO: support real product edit in future
            while (objectType == "product") {
                $node = getParentNode($node);
                objectType = getNodeObjectType($node);
            }
            */
            
            var actionProps = getActionProps($node, objectType, "edit");
            var objectId = getNodeObjectId($node);
            var data = $node.data;
            
            var params = {};
            if (objectType == "catalog") {
                params.prodCatalogId = objectId;
                params.productStoreId = this.productStoreId;
                // merge all, shouldn't be any conflicts...
                if (data.productStoreCatalogEntity) {
                    jQuery.extend(params, data.productStoreCatalogEntity);
                }
                if (data.prodCatalogEntity) {
                    jQuery.extend(params, data.prodCatalogEntity);
                }
            } else if (objectType == "category") {
                params.prodCatalogId = getNodeObjectId(getCatalogNodeForNode($node));
                params.productCategoryId = objectId;
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
            } else {
                // fail cleanly for now
                //reportInternalError("unsupported node object type for edit action: " + objectType);
            }
            
            execActionTarget($node, actionProps, params);
        };
        
        this.execRemoveForNode = function($node) {
            $node = checkGetNodeById($node);
            var objectType = getNodeObjectType($node);
            var actionProps = getActionProps($node, objectType, "remove");
            var objectId = getNodeObjectId($node);
            
            var params = {};
            if (objectType == "catalog") {
                params.productStoreId = this.productStoreId;
                params.prodCatalogId = objectId;
            } else if (objectType == "category") {
                params.productStoreId = this.productStoreId;
                params.productCategoryId = objectId;
                var parentNode = getParentNode($node);
                var parentType = getNodeObjectType(parentNode);
                if (parentType == "catalog") {
                    params.prodCatalogId = getNodeObjectId(parentNode);
                } else if (parentType == "category") {
                    params.parentProductCategoryId = getNodeObjectId(parentNode);
                } else {
                    reportInternalError("category node's parent is not catalog or category: " + objectId);
                }
            } else {
                // fail cleanly for now
                //reportInternalError("unsupported node object type for remove action: " + objectType);
            }
            
            execActionTarget($node, actionProps, params);
        };
        
        this.execNewCategoryForNode = function($node) {
            $node = checkGetNodeById($node);
            var objectType = getNodeObjectType($node);
            var actionProps = getActionProps($node, objectType, "newcategory");
            var objectId = getNodeObjectId($node);
            
            var params = {};
            if (objectType == "catalog") {
                params.productStoreId = this.productStoreId;
                params.prodCatalogId = objectId;
            } else if (objectType == "category") {
                params.productStoreId = this.productStoreId;
                if (objectType == "catalog") {
                    params.prodCatalogId = objectId;
                } else if (objectType == "category") {
                    params.parentProductCategoryId = objectId;
                } else {
                    reportInternalError("category node's parent is not catalog or category: " + objectId);
                }
            } else {
                // fail cleanly for now
                //reportInternalError("unsupported node object type for newcategory action: " + objectType);
            }
            
            execActionTarget($node, actionProps, params);
        };
        
        this.execManageForNode = function($node) {
            $node = checkGetNodeById($node);
            var objectType = getNodeObjectType($node);
            var actionProps = getActionProps($node, objectType, "manage");
            var objectId = getNodeObjectId($node);
            
            var params = {};
            if (objectType == "catalog") {
                params.prodCatalogId = objectId;
                params.productStoreId = this.productStoreId;
            } else if (objectType == "category") {
                params.productCategoryId = objectId;
                params.productStoreId = this.productStoreId;
            } else if (objectType == "product") {
                params.productId = objectId;
                params.productStoreId = this.productStoreId;
            } else {
                // fail cleanly for now
                //reportInternalError("unsupported node object type for manage action: " + objectType);
            }
            
            execActionTarget($node, actionProps, params);
        };
        
        this.execForNode = function(actionType, $node) {
            if (actionType === "edit") {
                this.execEditForNode($node);
            } else if (actionType === "remove") {
                this.execRemoveForNode($node);
            } else if (actionType === "newcategory") {
                this.execNewCategoryForNode($node);
            } else if (actionType === "manage") {
                this.execManageForNode($node);
            } else {
                reportInternalError("invalid action type requested for execForNode: " + actionType);
            }
        };
        
        <#-- 
            Menu plugs
        -->

        var getMenuActionDefs = function($node) {
            return {
                edit: {
                    "separator_before": false,
                    "separator_after": false,
                    "label": scth.labels.edit,
                    "action": function(obj) { <#-- FIXME?: should get node from obj, not $node? -->
                        scth.execEditForNode($node);
                    }
                },
                remove: {
                    "separator_before": false,
                    "separator_after": false,
                    "label": scth.labels.remove,
                    "action": function(obj) {
                        scth.execRemoveForNode($node);
                    }
                },
                newcategory: {
                    "separator_before": false,
                    "separator_after": false,
                    "label": scth.labels.newcategory,
                    "action": function(obj) {
                        scth.execNewCategoryForNode($node);
                    }
                },
                manage: {
                    "separator_before": true,
                    "separator_after": false,
                    "label": scth.labels.manage,
                    "action": function(obj) {
                        scth.execManageForNode($node);
                    }
                }
            };
        };
        
        var getMenuDefs = function($node) {
            var defs = getMenuActionDefs($node);
            var objectType = getNodeObjectType($node);
            
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
                if (isActionPropsValid(objectType, k)) {
                    resMenuDefs[k] = v;
                }
            });
            return resMenuDefs;
        };

        this.dropMenuHandler = function($node) {
            return getMenuDefs($node);
        };
        
        this.sideMenuHandler = function($node) {
            var $el = $("#ect-action-menu");
            var newOptions = getMenuDefs($node);

            $el.empty(); // remove old options
            $.each(newOptions, function(key, actionDef) {
                var newEl = $(scth.emptyMenuMarkup || '<ul></ul>');
                var menuAnchor = $(newEl).find('a:last-child');
                menuAnchor.attr("href","javascript:void(0);").text(actionDef.label);
                menuAnchor.click(actionDef.action);
                $el.append(newEl);
            });
        };
    }
    
    var ectHandler = new ScpCatalogTreeHandler({
        treeId: "${escapeVal(ectTreeId!, 'js')}",
        productStoreId: "${escapeVal(productStoreId!, 'js')}",
        actionProps: <@objectAsScript object=(ectActionProps!{}) lang='js'/>,
        hideShowFormIds: <@objectAsScript object=(ectAllHideShowFormIds![]) lang='js'/>,
        labels: {
            error: "${escapeVal(uiLabelMap.CommonError, 'js')}",
            edit: "${escapeVal(uiLabelMap.CommonEdit, 'js')}",
            remove: "${escapeVal(uiLabelMap.CommonRemove, 'js')}",
            newcategory: "${escapeVal(uiLabelMap.ProductNewCategory, 'js')}",
            manage: "${escapeVal(uiLabelMap.CommonManage, 'js')}"
        },
        emptyMenuMarkup: '<@compress_single_line><@menuitem type="link" href="" text=""/></@compress_single_line>'
    });
    
</@script>
<#assign treeEvents = {
    'select_node.jstree': 'ectHandler.sideMenuHandler(data.node);',
    'activate_node.jstree': 'ectHandler.execEditForNode(data.node);' <#-- TODO: REVIEW: is this too destructive? -->
    <#-- no longer supported by jstree
    'dblclick.jstree': 'ectHandler.execManageForNode(data.node);'-->
}/>

<#assign contextMenuPluginSettings = {
    "items": wrapRawScript("function(node) { return ectHandler.dropMenuHandler(node); }")
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


