
<#-- DEV NOTE: KEEP SETUP-SPECIFIC CONFIG OUT OF THIS FILE
    SO IT CAN BE FACTORED OUT LATER (TODO) -->

<#if !ectTreeId?has_content>
  <#assign ectTreeId = "ectTree_" + rawString(productStoreId!)>
</#if>

<#-- FIXME: private vs public methods are kind of arbitrary for now until code settles -->
<@script>
    
    /**
     * Catalog tree handler constructor.
     */
    function ScpCatalogTreeHandler(data) { // TODO?: this object could go in js file
        var scth = this; // capture for private methods and js kludges

        scth.fadeOptions = data.fadeOptions || {};
        scth.treeId = data.treeId;
        scth.productStoreId = data.productStoreId;
        scth.allActionProps = data.actionProps || {};
        scth.emptyMenuItemMarkup = data.emptyMenuItemMarkup;
        scth.postMenuItemMarkup = data.postMenuItemMarkup;
        scth.hideShowFormIds = data.hideShowFormIds;
        scth.labels = data.labels || {};
        scth.callbacks = data.callbacks || {};

        <#-- 
            Helpers
        -->
        
        var startsWith = function(str, prefix) {
            return (str.lastIndexOf(prefix, 0) === 0);
        };
        var endsWith = function(str, suffix) {
            return (str.indexOf(suffix, str.length - suffix.length) !== -1);
        };
    
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
        var getActionProps = function(objectType, actionName) {
            var actionGroup = scth.allActionProps[objectType];
            if (!isNonEmptyObj(actionGroup)) return {"type":"none"};
            var action = actionGroup[actionName];
            if (!isNonEmptyObj(action)) return {"type":"none"};
        
            // COPY the object because will be modified
            return jQuery.extend(true, {}, action);
        };
        
        /**
         * Renames and/or filters out parameters by name as requested by caller action props.
         * To prevent entries in paramNames from being included you must set paramNamesMode "explicit".
         * Reserved keys: "data"
         */
        var getResolvedActionPropsParams = function(ai, params) {
            var actionProps = ai.actionProps;
            var resParams = {};
            if (isUndefOrNull(params)) {
                params = actionProps.params;
            }
            if (isNonEmptyObj(params)) {
                var explicitOnly = (actionProps.paramNamesMode === "explicit"); // default: transfer all
            
                // this is to support forms/links that need different field names or copied to multiple fields
                if (isObj(actionProps.paramNames)) {
                    jQuery.each(params, function(k, v) {
                        if (k === 'data') {
                            resParams.data = v;
                        } else {
                            var replName = actionProps.paramNames[k];
                            if (!isUndefOrNull(replName)) {
                                if (jQuery.type(replName) === 'array') {
                                    jQuery.each(replName, function(i, e) {
                                        resParams[e] = v;
                                    });
                                } else {
                                    if (replName === true) {
                                        resParams[k] = v; // default
                                    } else if (jQuery.type(replName) !== 'boolean') { // boolean false prevents use
                                        resParams[replName] = v;
                                    }
                                }
                            } else {
                                if (!explicitOnly) {
                                    resParams[k] = v;
                                }
                            }
                        }
                    });
                } else {
                    if (!explicitOnly) {
                        resParams = jQuery.extend({}, params);
                    } else {
                        resParams = {data: params.data};
                    }
                }
            }
            return resParams;
        };
        
        /**
         * By default, clears all fields/elems having "etc-xxxfield" classes.
         */
        this.clearFormCommon = function(form, params, ai) {
            jQuery('.ect-inputfield', form).filter(':input').val('');
            jQuery('.ect-displayfield', form).html('');
            jQuery('.ect-managefield', form).html('');
        };
        
        this.makeManageLinkForElem = function(elem, name, value, form, params, ai) {
            if (value) {
                // FIXME: unhardcode markup
                var markup = jQuery('<a href="javascript:void(0);" class="ect-managefield-link">' + value + '</a>');
                markup.click(function() {
                    scth.execManageForNode(ai.node);
                });
                elem.html(markup);
            } else {
                elem.html('');
            }
        };
        
        this.populateFormFieldCommon = function(elem, name, value, params, form, ai) {
            if (isUndefOrNull(value)) {
                value = '';
            }
            if (elem.is(':input')) {
                elem.val(value);
            } else if (elem.hasClass('ect-displayfield')) {
                elem.html(value);
            } else if (elem.hasClass('ect-managefield')) {
                scth.makeManageLinkForElem(elem, name, value, form, params, ai);
            } else {
                reportInternalError('form field misconfigured for use with catalog tree - no value can be assigned. form id: ' + 
                    elem.closest('form').prop('id') + 
                    ', elem tag: ' + elem.prop('tagName') +
                    ', class: ' + elem.attr('class') +
                    ',  name: ' + elem.prop('name'));
            }
        };
        
        var extractClassNameSuffix = function(elem, prefix) {
            var classes = elem.attr('class').split(/\s+/);
            var result = null;
            jQuery.each(classes, function(i, e) {
                if (startsWith(e, prefix)) {
                    result = e.substring(prefix.length);
                    return false;
                }
            });
            return result;
        };
        
        this.getEctFormFieldName = function(elem) {
            var name = null;
            if (elem.is(':input')) {
                name = elem.prop('name');
            } else if (elem.hasClass('ect-displayfield')) {
                name = extractClassNameSuffix(elem, 'ect-displayfield-for-'); 
            } else if (elem.hasClass('ect-managefield')) {
                name = extractClassNameSuffix(elem, 'ect-managefield-for-'); 
            } 
            if (!name) {
                reportInternalError('form field misconfigured for use with catalog tree' +
                    ' - no name can be extracted from it. form id: ' + elem.closest('form').prop('id') + 
                    ', elem tag: ' + elem.prop('tagName') +
                    ', class: ' + elem.attr('class') +
                    ',  name: ' + elem.prop('name'));
            }
            return name;
        };
        
        /**
         * default populate form implementation.
         * each form field/elem with "ect-xxxclass" receives a param or empty value/html.
         */
        this.populateFormCommon = function(form, params, ai) {
            if (isObj(params)) {
                var fieldHandlers = ai.actionProps.populateFormFields || {};
                
                jQuery('.ect-inputfield, .ect-displayfield, .ect-managefield', form).each(function(i, elem) {
                    elem = jQuery(elem);
                    var name = scth.getEctFormFieldName(elem);
                    if (name) {
                        var value = params[name];
                        
                        var execCommon = true;
                        if (fieldHandlers[name]) {
                            execCommon = fieldHandlers[k](elem, name, value, form, params, ai);
                            if (execCommon !== false) {
                                execCommon = true;
                            }
                        }
                        if (execCommon) {
                            scth.populateFormFieldCommon(elem, name, value, form, params, ai);
                        }
                    }
                });
            }
        };
        
        var populateForm = function(form, params, ai) {
            if (ai.actionProps.clearForm !== false) {
                var execClearCommon = true; 
                if (jQuery.type(ai.actionProps.clearForm) === 'function') {
                    execClearCommon = ai.actionProps.clearForm(form, params, ai);
                    if (execClearCommon !== false) {
                        execClearCommon = true;
                    }
                }
                if (execClearCommon) {
                    scth.clearFormCommon(form, params, ai);
                }
            }
            var execCommon = true; 
            if (jQuery.type(ai.actionProps.populateForm) === 'function') {
                execCommon = ai.actionProps.populateForm(form, params, ai);
                if (execCommon !== false) {
                    execCommon = true;
                }
            } 
            if (execCommon) {
                scth.populateFormCommon(form, params, ai);
            }
        };
        
        // standard action target implementation (TODO?: callbacks or override into this)
        // NOTE: caller can pass params and call setActionPropsParams instead (convenience)
        var execActionTarget = function(ai, params) {
            params = getResolvedActionPropsParams(ai, params);
            if (ai.actionProps.type == "link") {
                openLink(ai.actionProps.url, params, ai.actionProps.target);
            } else if (ai.actionProps.type == "form") {
                var form = jQuery('#' + ai.formId);
                if (form.length) {
                    if (form.prop('tagName').toLowerCase() !== "form") {
                        form = jQuery('form', form); // find first child that is a form (convenience)
                    }
                    if (form.length) {
                        form = form.first();
                        if (ai.actionProps.mode == "show") {
                            populateForm(form, params, ai);
                            if (scth.callbacks.showFormActivated) {
                                scth.callbacks.showFormActivated(form, ai);
                            }
                            if (scth.hideShowFormIds) {
                                jQuery.each(scth.hideShowFormIds, function(i, e) {
                                    jQuery('#'+e).fadeOut(scth.fadeOptions);
                                });
                            }
                            jQuery('#'+ai.containerId).fadeIn(scth.fadeOptions);
                        } else if (ai.actionProps.mode == "submit") {
                            var doExec = true;
                            if (ai.actionProps.confirmMsg) {
                                doExec = askConfirm(ai.actionProps.confirmMsg);
                            }
                            if (doExec) {
                                populateForm(form, params, ai);
                                form.submit();
                            }
                        } else {
                            reportInternalError("invalid form action mode: " + ai.actionProps.mode);
                        }
                    } else {
                        reportInternalError("could not find form for form or container id: " + ai.formId);
                    }
                } else {
                    reportInternalError("bad form or container id: " + ai.formId);
                }
            } else if (ai.actionProps.type && ai.actionProps.type != "none") {
                reportInternalError("invalid action type: " + ai.actionProps.type);
            }
        };
 
        /**
         * Merges the entity fields for the node together.
         */
        this.getNodeEntitiesMerged = function($node) {
            var params = {};
            jQuery.each($node.data || {}, function(k, v) {
                if (endsWith(k, 'Entity')) {
                    jQuery.extend(params, v);
                }
            });
            return params;
        };
 
        /**
         * Prepares params for the action, for links & form filling. 
         * These get filtered later.
         * Entity values are automatically merged into the params map, works ok in most cases.
         * Fields try to use "most common" names by default, but actionProps config can
         * rename them after.
         */
        var makeParamsMap = function(ai, mergeEntities) {
            var params = {};
            
            // store & catalog (these not necessarily included in entities)
            params.productStoreId = scth.productStoreId;
            if (ai.objectType == "catalog") {
                params.prodCatalogId = ai.objectId;
            } else if (ai.node) {
                params.prodCatalogId = getNodeObjectId(getCatalogNodeForNode(ai.node))
            }
            
            // merge any *Entity fields into params map (this takes care of objectId & parent id)
            if (mergeEntities !== false && ai.node) {
                jQuery.extend(params, scth.getNodeEntitiesMerged(ai.node));
            }
            
            params.data = ai.data; // special entry
            return params;
        };

        <#-- 
            Core functions
        -->
        
        /**
         * Action info object, also accessible from most custom callbacks ("ai" parameter).
         */
        var ActionInfo = function($node, actionType, objectType) {
            $node = checkGetNodeById($node);
            this.objectType = objectType || getNodeObjectType($node);
            this.scth = scth; // for outside code
            if ($node) {
                this.node = $node;
                this.data = $node.data || {};
                this.objectId = getNodeObjectId($node);
            } else {
                this.data = {};
            }
            this.actionType = actionType;
            this.actionProps = getActionProps(this.objectType, this.actionType);
            this.formId = this.actionProps.formId || this.actionProps.id;
            this.containerId = this.actionProps.id;
            // TODO: more accessors
        }
        
        this.execEditForNode = function($node) {
            var ai = new ActionInfo($node, "edit");
            var params = makeParamsMap(ai);
            // default params OK
            return execActionTarget(ai, params);
        };
        
        this.execRemoveAssocForNode = function($node) {
            var ai = new ActionInfo($node, "removeassoc");
            var params = makeParamsMap(ai);
            // default params OK
            return execActionTarget(ai, params);
        };
        
        this.execRemoveForNode = function($node) {
            var ai = new ActionInfo($node, "remove");
            var params = makeParamsMap(ai);
            // default params OK
            return execActionTarget(ai, params);
        };
        
        this.execNewCategoryForNode = function($node) {
            var ai = new ActionInfo($node, "newcategory");
            var params = makeParamsMap(ai);
            // default params OK
            return execActionTarget(ai, params);
        };
        
        this.execNewCatalog = function() {
            var ai = new ActionInfo(null, "newcatalog", "default");
            var params = makeParamsMap(ai);
            // default params OK
            return execActionTarget(ai, params);
        };
        
        this.execManageForNode = function($node) {
            var ai = new ActionInfo($node, "manage");
            var params = makeParamsMap(ai);
            // default params OK
            return execActionTarget(ai, params);
        };
        
        this.execForNode = function(actionType, $node) {
            if (actionType === "edit") {
                return this.execEditForNode($node);
            } else if (actionType === "removeassoc") {
                return this.execRemoveAssocForNode($node);
            } else if (actionType === "remove") {
                return this.execRemoveForNode($node);
            } else if (actionType === "newcategory") {
                return this.execNewCategoryForNode($node);
            } else if (actionType === "newcatalog") {
                return this.execNewCatalog();
            } else if (actionType === "manage") {
                return this.execManageForNode($node);
            } else {
                reportInternalError("invalid action type requested for execForNode: " + actionType);
                return undefined;
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
                    "action": function(obj) {
                        scth.execEditForNode($node);
                    }
                },
                removeassoc: {
                    "separator_before": false,
                    "separator_after": false,
                    "label": scth.labels.removeassoc,
                    "action": function(obj) {
                        scth.execRemoveAssocForNode($node);
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
                },
                newcatalog: {
                    "separator_before": false,
                    "separator_after": false,
                    "label": scth.labels.newcatalog,
                    "action": function(obj) {
                        scth.execNewCatalog();
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
                    "removeassoc": defs.removeassoc,
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
                var newEl = $(scth.emptyMenuItemMarkup || '<li></li>');
                var menuAnchor = $(newEl).find('a:last-child');
                menuAnchor.attr("href","javascript:void(0);").text(actionDef.label);
                menuAnchor.click(actionDef.action);
                $el.append(newEl);
            });
            if (scth.postMenuItemMarkup) {
                $el.append(scth.postMenuItemMarkup);
            }
        };
    }
    
    <#if !ectEmptyMenuItemMarkup?has_content>
      <#assign ectEmptyMenuItemMarkup><@compress_single_line><@menuitem type="link" 
        href="" text=""/></@compress_single_line></#assign>
    </#if>
    <#if !ectPostMenuItemMarkup??>
      <#assign ectPostMenuItemMarkup><@compress_single_line><@menuitem type="link" 
        href="javascript:void(0);" onClick="ectHandler.execNewCatalog();" text=uiLabelMap.ProductNewCatalog/></@compress_single_line></#assign>
    </#if>
    
    var ectHandler = new ScpCatalogTreeHandler({
        treeId: "${escapeVal(ectTreeId!, 'js')}",
        productStoreId: "${escapeVal(productStoreId!, 'js')}",
        actionProps: <@objectAsScript object=(ectActionProps!{}) lang='js'/>,
        hideShowFormIds: <@objectAsScript object=(ectAllHideShowFormIds![]) lang='js'/>,
        labels: {
            error: "${escapeVal(uiLabelMap.CommonError, 'js')}",
            edit: "${escapeVal(uiLabelMap.CommonEdit, 'js')}",
            removeassoc: "${escapeVal(uiLabelMap.CommonRemoveAssoc, 'js')}",
            remove: "${escapeVal(uiLabelMap.CommonRemove, 'js')}",
            newcategory: "${escapeVal(uiLabelMap.ProductNewCategory, 'js')}",
            newcatalog: "${escapeVal(uiLabelMap.ProductNewCatalog, 'js')}",
            manage: "${escapeVal(uiLabelMap.CommonManage, 'js')}"
        },
        emptyMenuItemMarkup: '${ectEmptyMenuItemMarkup}',
        postMenuItemMarkup: '${ectPostMenuItemMarkup}',
        callbacks: <@objectAsScript object=(ectCallbacks!{}) lang='js'/>
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
    
      <#-- MAIN JSTREE -->
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
    
      <#-- ACTIONS MENU -->
      <@section title=uiLabelMap.CommonActions><#-- DEV NOTE: why this? please add comment if you know: id="ect-action-menu-offset" -->
        <#if ectEmptyMenuMarkup?has_content>
          ${ectEmptyMenuMarkup}
        <#else>
          <ul class="side-nav ect-action-menu" id="ect-action-menu">
            <#if ectPostMenuItemMarkup?has_content>
              ${ectPostMenuItemMarkup}
            </#if>
          </ul>
        </#if>
      </@section>
      
      <#-- DISPLAY EXTRAS (OPTIONS, ETC.) -->
      <#if ectExtrasArea??>
        <#if ectExtrasArea?is_directive>
          <@ectExtrasArea/>
        <#else>
          ${ectExtrasArea}
        </#if>
      </#if>
      
    </@cell>
</@row>


