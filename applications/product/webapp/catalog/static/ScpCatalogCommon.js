/**
 * SCIPIO: Common catalog JS, catalog tree (ect) and related form-building core classes.
 * DEV NOTE: do not add any actions on file run (e.g. document ready), classes and functions only.
 */

/**
 * Catalog tree handler constructor.
 * REQUIRES: 
 * * /content/images/ScpContentCommon.js
 */
function ScpCatalogTreeHandler(data) { // TODO?: this object could go in js file
    var scth = this; // capture for private methods and js kludges
    var slfh = stcLocFieldHandler;
    
    scth.fadeOptions = data.fadeOptions || {};
    scth.treeId = data.treeId;
    scth.productStoreEntity = data.productStoreEntity;
    scth.productStoreId = data.productStoreEntity.productStoreId;
    scth.allActionProps = data.actionProps || {};
    scth.markup = new ScpCntFormMarkup(data.markupSelectors || {});
    scth.links = data.links || {};
    scth.hideShowFormIds = data.hideShowFormIds;
    scth.labels = data.labels || {};
    scth.callbacks = data.callbacks || {};
    scth.targetNodeInfo = data.targetNodeInfo;
    scth.eventStates = data.eventStates || {};
    scth.submittedFormId = data.submittedFormId;
    scth.initialParams = data.initialParams;
    scth.initialSettings = data.initialSettings || {};
    scth.popupMsgModalId = data.popupMsgModalId;
    scth.confirmMsgModalId = data.confirmMsgModalId;
    scth.dialogIdPrefix = data.dialogIdPrefix;
    scth.catalogLocFieldsInfo = data.catalogLocFieldsInfo || {};
    
    // workaround flags
    // FIXME: these are being used to prevent form changes on event error,
    // but relies on page to show correct initial form; should make pure JS solution
    var specFlags = {
        noShowFormChange: false,
        noShowFormPopulate: false
    };
    
    /*
     * Helpers
     */
     
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
    var ensureArray = function(valOrArray) {
        return jQuery.isArray(valOrArray) ? valOrArray : (isUndefOrNull(valOrArray) ? [] : [valOrArray]);
    };
    var htmlMarkupEscape = function(value) {
        return jQuery('<div></div>').text(value).html();
    };

    var startsWith = function(str, prefix) {
        return (str.lastIndexOf(prefix, 0) === 0);
    };
    var endsWith = function(str, suffix) {
        return (str.indexOf(suffix, str.length - suffix.length) !== -1);
    };
    var extractClassNameSuffix = function(elem, prefix) {
        return ScpCntFormUtil.extractClassNameSuffix(elem, prefix);
    };

    // TODO: REVIEW: these
    var openModal = function(modalElem) {
        try {
            modalElem.foundation('reveal', 'open');
        } catch(err) {
            try {
                modalElem.modal('show'); 
            }
            catch(err) {
                //t.dispatchEvent(event); // FIXME?
            }
        }
    };
    var closeModal = function(modalElem) {
        try {
            modalElem.foundation('reveal', 'close');
        } catch(err) {
            try {
                modalElem.modal('hide'); 
            }
            catch(err) {
                //t.dispatchEvent(event); // FIXME?
            }
        }
    };

    // TODO?: blocking mode? return values? callback?
    var showPopupMsg = function(msg, extraMsg) {
        if (scth.popupMsgModalId) {
            var modalElem = jQuery('#'+scth.popupMsgModalId);
            if (modalElem.length) {
                jQuery('.ect-dialogmsg', modalElem).html(msg);
                jQuery('.ect-dialogextramsg', modalElem).html(extraMsg || '');
                openModal(modalElem);
            } else {
                return alert(msg);
            }
        } else {
            return alert(msg);
        }
    };
    var showConfirmMsg = function(msg, extraMsg, modalElem, continueCallback) {
        if ((!modalElem || !modalElem.length) && scth.confirmMsgModalId) {
            modalElem = jQuery('#'+scth.confirmMsgModalId);
        }
        if (modalElem && modalElem.length) {
            jQuery('.ect-dialogmsg', modalElem).html(msg);
            jQuery('.ect-dialogextramsg', modalElem).html(extraMsg || '');
            jQuery('.ect-dialogbtn', modalElem).click(function() {
                closeModal(modalElem);
                var selectedName = extractClassNameSuffix(jQuery(this), 'ect-dialogbtn-');
                continueCallback(selectedName);
            });
            openModal(modalElem);
        } else {
            var result = confirm(msg);
            if (result) {
                continueCallback();
            }
        }
    };
    var reportError = function(msg) {
        alert(scth.labels.error + ": " + msg);
    };
    var reportInternalError = function(msg) {
        alert("Internal error: " + msg);
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
        if ($node && $node.data) return $node.data.li_attr.original_id;
        else return null;
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
    var getRootNode = function() {
        return getJsTree().get_node("#");
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
        if ($node && $node.data) return $node.data.type; // same names, 1-for-1 mapping
        else return null;
    };
    var getChildNodeByObjectId = function($node, objectId, targetObjectType) {
        var result = null;
        var tree = getJsTree();
        if ($node.children) {
            jQuery($node.children).each(function() {
                var $child = tree.get_node(this);
                if (objectId === getNodeObjectId($child) && 
                    (!targetObjectType || targetObjectType === getNodeObjectType($child))) {
                    result = $child;
                    return false;
                }
            });
        }
        return result;
    };
    var getNodeByObjectIdPath = function(objectIdList, targetObjectType, allowPartial) {
        var $node = getRootNode();
        for(var i = 0; i < objectIdList.length; i++) {
            var objectId = objectIdList[i];
            var $nextNode = null;
            if (i == (objectIdList.length - 1)) {
                // last node, must check type
                $nextNode = getChildNodeByObjectId($node, objectId, targetObjectType);
            } else {
                // middle node, no type check needed
                $nextNode = getChildNodeByObjectId($node, objectId, null);
            }
            if ($nextNode) {
                $node = $nextNode;
            } else {
                if (allowPartial === true) {
                    return $node;
                } else {
                    return null;
                }
            }
        }
        return $node;
    };
    var getNodeObjectIdPathList = function($node, includeLeafType) {
        if (!$node) return null;
        var idList = [];
        var leaf = true;
        while($node && !isRootNode($node)) {
            var id = getNodeObjectId($node);
            if (leaf) {
                var nodeType = getNodeObjectType($node);
                if (nodeType && includeLeafType !== false) id += "#" + nodeType;
                leaf = false;
            }
            idList.push(id);
            $node = getParentNode($node);
        }
        idList.reverse();
        return idList;
    };
    var getNodeObjectIdPathString = function($node, includeLeafType) {
        var pathList = getNodeObjectIdPathList($node, includeLeafType);
        if (isUndefOrNull(pathList)) return null;
        return pathList.join('/');
    };
    var isNodeObjectParent = function($node) {
        if (typeof $node.data.isParent === 'boolean') {
            return $node.data.isParent;
        } else {
            // fallback
            return getJsTree().is_parent($node);
        }
    };
    var isChildNodeOf = function($node, $childNode) {
        if ($childNode) $childNode = getParentNode($childNode); // same should not count
        while($childNode && !isRootNode($childNode)) {
            if ($childNode.id === $node.id) return true;
            $childNode = getParentNode($childNode);
        }
        return false;
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
    var getActionPropsDefaultParams = function(actionProps) {
        if (!actionProps) return {};
        else if (typeof actionProps.defaultParams === 'function') {
            return actionProps.defaultParams();
        } else {
            return actionProps.defaultParams || {};
        }
    };
    
    // NOTE: does not include any info about the association
    var getNodeObjectDesc = function($node) {
        var desc = null;
        var objectType = getNodeObjectType($node);
        //var objectId = getNodeObjectId($node);
        if (objectType === 'catalog') {
            desc = $node.text + ' (' + scth.labels.catalog + ')';
        } else if (objectType === 'category') {
            desc = $node.text + ' (' + scth.labels.category + ')';
        } else if (objectType === 'product') {
            desc = $node.text + ' (' + scth.labels.product + ')';
        } else if ($node && $node.text) { // fallback
            desc = $node.text;
        }
        return desc;
    };
    var getStoreObjectDesc = function() { // TODO: version that works without node
        var desc = null;
        if (scth.productStoreEntity) {
            desc = scth.productStoreEntity.storeName + ' [' + scth.productStoreEntity.productStoreId + '] (' + scth.labels.store + ')';
        }
        return desc;
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
    
    var setCheckboxChecked = function(field, state) {
        if (field.is("input[type=checkbox]")) {
            field.prop('checked', state);
        } else {
            jQuery("input[type=checkbox]", field).prop('checked', state);
        }
    };
    
    /**
     * By default, clears all fields/elems having "etc-xxxfield" classes.
     */
    this.clearFormCommon = function(form, params, ai) {
        jQuery('.ect-inputfield', form).filter(':input').val('');
        jQuery('.ect-displayfield', form).html('');
        jQuery('.ect-managefield', form).html('');
        jQuery('.ect-checkfield', form).each(function() {
            var field = jQuery(this);
            if (field.hasClass('ect-initial-checked')) {
                setCheckboxChecked(field, true);
            } else {
                setCheckboxChecked(field, false);
            }
        });
        
        if (params.local) {
            var localizedFields = params.local.localizedFields;
            if (localizedFields) {
                slfh.removeAllLocalizedFieldEntries(form, localizedFields.typeNames, localizedFields.entryDataListsByType);
            }
        }
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
            value = ai.defaultParams[name] || '';
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
    
    this.getEctFormFieldName = function(elem) {
        var name = null;
        if (elem.is(':input')) {
            name = elem.prop('name');
            if (!name) {
                if (elem.hasClass('ect-inputfield')) {
                    name = extractClassNameSuffix(elem, 'ect-inputfield-for-'); 
                }
            }
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
     * Default populate form implementation.
     * Each form field/elem with "ect-xxxclass" receives a param or empty value/html.
     * TODO?: special handling for "ect-nonvaluefield" class, not yet established.
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
            
            if (params.local) {
                var localizedFields = params.local.localizedFields;
                if (localizedFields) {
                    slfh.rebuildAllLocalizedFieldEntries(form, localizedFields.typeNames, localizedFields.entryDataListsByType);
                }
            }
        }
    };
    
    var getCommonTreeFields = function(form, params, ai) {
        return {
            ectTargetNodePath: getNodeObjectIdPathString(ai.node), // the "current" node path
            ectNewTargetNodePath: params.ectNewTargetNodePath, // the "next" node path IF event success (must be set by callers)
            ectSubmittedFormId: form.prop('id')
        };
    };
    
    var populateFormCommonTreeFieldsOnly = function(form, params, ai) {
        var fields = getCommonTreeFields(form, params, ai);
        jQuery('input[name=ectTargetNodePath].ect-inputfield', form).val(fields.ectTargetNodePath || '');
        jQuery('input[name=ectNewTargetNodePath].ect-inputfield', form).val(fields.ectNewTargetNodePath || '');
        jQuery('input[name=ectSubmittedFormId].ect-inputfield', form).val(fields.ectSubmittedFormId || '');
    };
    
    var populateForm = function(form, params, ai) {
        if (ai.actionProps.mode == "show" && specFlags.noShowFormPopulate === true) {
            // still have to populate common fields (now below)
            //populateFormCommonTreeFieldsOnly(form, params, ai); // doing at end always for now...
        } else {
            // (now below)
            //params = jQuery.extend({}, params, getCommonTreeFields(form, params, ai));
        
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
        }
        // FIXME?: inconsistent population code with the rest, but works for now
        populateFormCommonTreeFieldsOnly(form, params, ai);
    };
    
    // TODO: REVIEW: this may need to be triggered earlier in some cases, meaning should be able to work without "ai"...
    var checkExecConfirm = function(ai, params, preParamNamesMap, execCallback) {
        // FIXME?: the confirm message should occur earlier than this, but need new factor points
        var confirmMsg = params.local.confirmMsg || ai.actionProps.confirmMsg;
        if (confirmMsg) {
            var confirmExtraMsg = params.local.confirmExtraMsg || ai.actionProps.confirmExtraMsg;
            var modalElem = jQuery('#'+ scth.dialogIdPrefix + ai.objectType + '-' + ai.actionType);
            showConfirmMsg(confirmMsg, confirmExtraMsg, modalElem, function(subActionType) {
                params.subActionType = subActionType;
                if (preParamNamesMap && preParamNamesMap.subActionType) {
                    if (typeof preParamNamesMap.subActionType === 'function') {
                        preParamNamesMap.subActionType(subActionType, params, ai); 
                    } else {
                        params[preParamNamesMap.subActionType] = subActionType;
                    }
                }
                
                // check if the modal had any params, dump them into params
                jQuery('form.ect-dialogopts-form :input', modalElem).each(function(i, input) {
                    input = jQuery(input);
                    var name = input.prop('name');
                    if (name) params[name] = input.val();
                });
                
                execCallback();
            });
        } else {
            if (!isUndefOrNull(confirmMsg)) {
                reportInternalError('dialog confirm message appears empty (bad label?) - aborting operation');
            } else {
                execCallback();
            }
        }
    };
    
    /** 
     * Standard action target implementation (TODO?: callbacks or override into this).
     * NOTE: caller can pass params and call setActionPropsParams instead (convenience).
     */
    var execActionTarget = function(ai, params) {
        var coreExec = function() {
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
                            // abort form change and populate in special cases 
                            if (specFlags.noShowFormChange === true) {
                                ;
                            } else {
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
                            }
                        } else if (ai.actionProps.mode == "submit") {
                            populateForm(form, params, ai);
                            form.submit();
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
            coreExec();
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
 
    var substituteMsg = function(msg, values, quoteChar, useHtml, htmlEscapeValues) {
        if (msg && values) {
            if (quoteChar === false) quoteChar = '';
        else if (!quoteChar) quoteChar = "'";
        
        var spanOpen = '<span class="ect-dialogmsg-recordname">';
        var spanClose = '</span>';
        if (useHtml === false) {
            spanOpen = '';
            spanClose = '';
        }

        jQuery.each(values, function(k, value) {
            if (value) {
                value = quoteChar+value+quoteChar;
                if (htmlEscapeValues !== false) {
                    value = htmlMarkupEscape(value);
                }
                value = spanOpen+value+spanClose;
                msg = msg.replace('${'+k+'}', value);
                }
            });
        }
        return msg;
    };
 
    var substituteConfirmMsg = function(ai, params, values, quoteChar, useHtml, htmlEscapeValues) {
        var confirmMsg = params.local.confirmMsg || ai.actionProps.confirmMsg;
        confirmMsg = substituteMsg(confirmMsg, values, quoteChar, useHtml, htmlEscapeValues);
        params.local.confirmMsg = confirmMsg;
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
        params.local = {}; // special entry
        
        // TODO: the confirmMsg should be pre-parsed so that the text in _between_ dollar-sign variables is html-escaped...
        //var confirmMsg = params.local.confirmMsg || ai.actionProps.confirmMsg;
        //...
        //params.local.confirmMsg = confirmMsg;
        
        substituteConfirmMsg(ai, params, {
            recordName: getNodeObjectDesc(ai.node)
        });
        
        return params;
    };

    var runAjax = function(url, reqParams, successCb) {
        jQuery.ajax({
            url: url,
            data: reqParams,
            async: true,
            type: "POST",
            success: function(data) {
                if (data._ERROR_MESSAGE_ || data._ERROR_MESSAGE_LIST_) {
                    if (data._ERROR_MESSAGE_) {
                        reportError(scth.labels.errorfromserver + ': ' + data._ERROR_MESSAGE_);
                    } else {
                        reportError(scth.labels.errorfromserver + ': ' + data._ERROR_MESSAGE_LIST_[0]);
                    }
                } else {
                    successCb(data);
                }
            },
            error: function() {
                reportError(scth.labels.servercommerror);
            }
        });   
    };

    /*
     * Core functions
     */
    
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
        this.defaultParams = getActionPropsDefaultParams(this.actionProps);
    };
    
    var getActionInfo = function($node, actionType, objectType) {
        var ai = new ActionInfo($node, actionType, objectType);
        return ai;
    };
    
    var initLocFieldParams = function(params, objectType) {
        var fieldInfo = scth.catalogLocFieldsInfo[objectType];
        if (fieldInfo) {
            params.local.localizedFields = {
                typeNames: fieldInfo.typeNames,      
                entryDataListsByType: {}
            };
        }
        return fieldInfo;
    };
    
    this.execEditForNode = function($node) {
        var ai = getActionInfo($node, "edit");
        var params = makeParamsMap(ai);
        // default params OK
        
        checkExecConfirm(ai, params, {}, function() {
            var execEdit = function() {
                execActionTarget(ai, params);
            };
            
            var doExecEdit = true;
            if (specFlags.noShowFormPopulate !== true) {
                var fieldInfo = initLocFieldParams(params, ai.objectType);
                if (fieldInfo) {
                    if (ai.objectType === 'category') {
                        if (scth.links.getProductCategoryExtendedData) {
                            doExecEdit = false;
                            runAjax(scth.links.getProductCategoryExtendedData, {
                                    productCategoryId: ai.objectId,
                                    prodCatContentTypeIdList: fieldInfo.typeNameListStr,
                                    getViewsByType: true,
                                    getTextByTypeAndLocale: false
                                }, 
                                function(data) {
                                    if (data.viewsByType) {
                                        //alert('server result: ' + JSON.stringify(data.textByTypeAndLocale, null, 2));
                                        params.local.localizedFields.entryDataListsByType = slfh.parseViewsByType(data.viewsByType);
                                    }
                                    execEdit();
                                }
                            );
                        }
                    } else if (ai.objectType === 'product') {
                        if (scth.links.getProductExtendedData) {
                            doExecEdit = false;
                            runAjax(scth.links.getProductExtendedData, {
                                    productId: ai.objectId,
                                    productContentTypeIdList: fieldInfo.typeNameListStr,
                                    getViewsByType: true,
                                    getTextByTypeAndLocale: false
                                }, 
                                function(data) {
                                    if (data.viewsByType) {
                                        //alert('server result: ' + JSON.stringify(data.textByTypeAndLocale, null, 2));
                                        params.local.localizedFields.entryDataListsByType = slfh.parseViewsByType(data.viewsByType);
                                    }
                                    execEdit();
                                }
                            );
                        }
                    }
                }
            }
            if (doExecEdit) {
                execEdit();
            }
        });
    };
    
    this.areNodesDraggable = function($nodeOrList) {
        $nodeOrList = ensureArray($nodeOrList);
        if (!$nodeOrList || $nodeOrList.length <= 0) return false;
        var draggable = true;
        jQuery.each($nodeOrList, function(i, $node) {
            var objectType = getNodeObjectType($node);
            if (objectType !== 'category' && objectType !== 'product') {
                draggable = false;
                return false;
            }
        });
        return draggable;
    };
    
    /**
     * Returns true only if all nodes are valid for copy/move to the target.
     */
    this.isValidCopyMoveTarget = function($nodeOrList, $targetNode) {
        if (!$nodeOrList || !$targetNode || isRootNode($targetNode)) return false;
        $nodeOrList = ensureArray($nodeOrList);
        if ($nodeOrList.length <= 0) return false;
        
        var targetObjectId = getNodeObjectId($targetNode);
        var targetType = getNodeObjectType($targetNode);
        if (targetType !== 'catalog' && targetType !== 'category') return false;
        
        var result = true;
        for(var i=0; i < $nodeOrList.length; i++) {
            var $node = $nodeOrList[i];
        
            if (!$node || isRootNode($node)) return false;
            
            var objectType = getNodeObjectType($node);
            if (objectType !== 'category' && objectType !== 'product') return false;
            
            var $parent = getParentNode($node);
            if (!$parent) return false;
            
            // check if target is already us (?!) or our parent
            if ($node.id === $targetNode.id || $node.id === $parent.id) return false;
            // must also check via "real" object ID
            var objectId = getNodeObjectId($node);
            if (objectType === targetType && objectId === targetObjectId) return false;
            var parentType = getNodeObjectType($parent);
            var parentObjectId = getNodeObjectId($parent);
            if (parentType === targetType && parentObjectId === targetObjectId) return false;
            
            if (objectType === 'product') {
                // products can only go under categories
                if (targetType !== 'category') return false;
            } else {
                // category can't go under itself
                if (isChildNodeOf($node, $targetNode)) return false;
            }
            
            // make sure the object is not already child of the target
            
            if (getChildNodeByObjectId($targetNode, objectId, objectType)) return false;
        }
        return true;  
    };
    
    this.execCopyMoveAssocForNode = function($node, $targetNode) {
        if (!scth.isValidCopyMoveTarget($node, $targetNode)) {
            return false;
        }
    
        var ai = getActionInfo($node, "copymoveassoc");
        var params = makeParamsMap(ai);
        
        // target to_ node
        var targetObjectType = getNodeObjectType($targetNode);
        if (targetObjectType === 'catalog') {
            params.to_prodCatalogId = getNodeObjectId($targetNode);
            // if already a catalog relation, preserve the type,
            // otherwise use caller default (user can edit after)...
            if (params.prodCatalogCategoryTypeId) {
                params.to_prodCatalogCategoryTypeId = params.prodCatalogCategoryTypeId;
            } else {
                // get default from catalog newcategory defaultParams
                var cncDefParams = getActionPropsDefaultParams(getActionProps("catalog", "newcategory"));
                if (cncDefParams.prodCatalogCategoryTypeId) {
                    params.to_prodCatalogCategoryTypeId = cncDefParams.prodCatalogCategoryTypeId;
                } else {
                    params.to_prodCatalogCategoryTypeId = "PCCT_BROWSE_ROOT"; // fallback default (can't be empty)
                }
            }
            params.to_parentProductCategoryId = null;
        } else if (targetObjectType === 'category') {
            params.to_prodCatalogId = null;
            params.to_prodCatalogCategoryTypeId = null;
            params.to_parentProductCategoryId = getNodeObjectId($targetNode);
        }
        params.to_fromDate = null; // TODO?: no way to populate - service will make it
        params.to_sequenceNum = null; // TODO?: no way to populate (unreliable), can't reuse previous, but could be desirable...
        
        substituteConfirmMsg(ai, params, {
            toRecordName: getNodeObjectDesc($targetNode)
        });
        
        // NOTE: this path is used on success only and further adjusted server-side
        params.ectNewTargetNodePath = getNodeObjectIdPathString($targetNode);
        
        // FIXME: the checkExecConfirm should be earlier in function, but this is working for time being
        var effArgs = {};
        checkExecConfirm(ai, params, 
            {subActionType: function(subActionType, params, ai) {
                if ("copy" === subActionType) {
                    effArgs.ai = getActionInfo($node, "copyassoc");
                    params.deleteAssocMode = null;
                    params.returnAssocFields = "true"; // for now, switching to the new node after copy...
                } else if ("move-remove" === subActionType) {
                    effArgs.ai = getActionInfo($node, "moveassoc");
                    params.deleteAssocMode = "remove";
                    params.returnAssocFields = "true";
                } else if ("move-expire" === subActionType) {
                    effArgs.ai = getActionInfo($node, "moveassoc");
                    params.deleteAssocMode = "expire";
                    params.returnAssocFields = "true";
                } else {
                    // should not happen
                    reportInternalError("invalid copy/move sub action type: " + subActionType);
                }
            }},
            function() {
                if (effArgs.ai) {
                    execActionTarget(effArgs.ai, params);
                }
            }
        );
    };
    
    this.execRemoveAssocForNode = function($node) {
        var ai = getActionInfo($node, "removeassoc");
        var params = makeParamsMap(ai);
        
        var targetDesc;
        if (ai.objectType === 'catalog') {
            targetDesc = getStoreObjectDesc();
        } else {
            targetDesc = getNodeObjectDesc(getParentNode($node));
        }
        substituteConfirmMsg(ai, params, {
            toRecordName: targetDesc
        });
        
        // default params OK
        checkExecConfirm(ai, params, {subActionType:"deleteAssocMode"}, function() {
            if (params.deleteAssocMode) {
                execActionTarget(ai, params);
            } else {
                reportInternalError('removeassoc received no deleteAssocMode from dialog (confirm dialog error - aborting)');
            }
        });
    };
    
    this.execRemoveForNode = function($node) {
        var nodeObjectIsParent = isNodeObjectParent($node);
        if (nodeObjectIsParent) {
            showPopupMsg(substituteMsg(scth.labels.cannotremovehaschild, {recordName:getNodeObjectDesc($node)}));
            return false;
        }
    
        var ai = getActionInfo($node, "remove");
        var params = makeParamsMap(ai);
        // default params OK
        checkExecConfirm(ai, params, {}, function() {
            execActionTarget(ai, params);
        });
    };
    
    this.execNewProductForNode = function($node) {
        var ai = getActionInfo($node, "newproduct");
        
        // SPECIAL: the default entity merge doesn't work for this
        var params = makeParamsMap(ai, false);
        if (ai.objectType == "category") {
            params.productCategoryId = ai.data.productCategoryEntity.productCategoryId;
            // SPECIAL: we will set primaryProductCategoryId because it's the most common case,
            // although not guaranteed always wanted
            params.primaryProductCategoryId = params.productCategoryId;
            params.isVirtual = "N";
            params.isVariant = "N";
        }
        
        checkExecConfirm(ai, params, {}, function() {
            initLocFieldParams(params, ai.objectType);
            execActionTarget(ai, params);
        });
    };
    
    this.execNewCategoryForNode = function($node) {
        var ai = getActionInfo($node, "newcategory");
        
        // SPECIAL: the default entity merge doesn't work for this
        var params = makeParamsMap(ai, false);
        if (ai.objectType == "catalog") {
            // prodCatalogId will be ok
        } else if (ai.objectType == "category") {
            params.parentProductCategoryId = ai.data.productCategoryEntity.productCategoryId;
        }
        
        checkExecConfirm(ai, params, {}, function() {
            initLocFieldParams(params, ai.objectType);
            execActionTarget(ai, params);
        });
    };
    
    this.execNewCatalog = function() {
        var ai = getActionInfo(null, "newcatalog", "default");
        var params = makeParamsMap(ai);
        // default params OK
        checkExecConfirm(ai, params, {}, function() {
            initLocFieldParams(params, ai.objectType);
            execActionTarget(ai, params);
        });
    };
    
    this.execAddProductForNode = function($node) {
        var ai = getActionInfo($node, "addproduct");
        var params = makeParamsMap(ai);
        // default params OK
        checkExecConfirm(ai, params, {}, function() {
            execActionTarget(ai, params);
        });
    };
    
    this.execAddCategoryForNode = function($node) {
        var ai = getActionInfo($node, "addcategory");
        
        // SPECIAL: the default entity merge doesn't work for this
        var params = makeParamsMap(ai, false);
        if (ai.objectType == "catalog") {
            // prodCatalogId will be ok
        } else if (ai.objectType == "category") {
            params.parentProductCategoryId = ai.data.productCategoryEntity.productCategoryId;
        }
        
        checkExecConfirm(ai, params, {}, function() {
            execActionTarget(ai, params);
        });
    };
    
    this.execAddCatalog = function() {
        var ai = getActionInfo(null, "addcatalog", "default");
        var params = makeParamsMap(ai);
        // default params OK
        checkExecConfirm(ai, params, {}, function() {
            execActionTarget(ai, params);
        });
    };
    
    this.execManageForNode = function($node) {
        var ai = getActionInfo($node, "manage");
        var params = makeParamsMap(ai);
        // default params OK
        checkExecConfirm(ai, params, {}, function() {
            execActionTarget(ai, params);
        });
    };
    
    this.execForNode = function(actionType, $node, $targetNode) {
        if (actionType === "edit") {
            return this.execEditForNode($node);
        } else if (actionType === "copymoveassoc") {
            return this.execCopyMoveAssocForNode($node, $targetNode);
        } else if (actionType === "removeassoc") {
            return this.execRemoveAssocForNode($node);
        } else if (actionType === "remove") {
            return this.execRemoveForNode($node);
        } else if (actionType === "newproduct") {
            return this.execNewProductForNode($node);
        } else if (actionType === "newcategory") {
            return this.execNewCategoryForNode($node);
        } else if (actionType === "newcatalog") {
            return this.execNewCatalog();
        } else if (actionType === "addproduct") {
            return this.execAddProductForNode($node);
        } else if (actionType === "addcategory") {
            return this.execAddCategoryForNode($node);
        } else if (actionType === "addcatalog") {
            return this.execAddCatalog();
        } else if (actionType === "manage") {
            return this.execManageForNode($node);
        } else {
            reportInternalError("invalid action type requested for execForNode: " + actionType);
            return undefined;
        }
    };
    
    /*
     * Menu plugs
     */

    var getMenuActionDefs = function($node) {
        var nodeObjectIsParent = isNodeObjectParent($node);
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
                "_disabled": nodeObjectIsParent,
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
            addcategory: {
                "separator_before": false,
                "separator_after": false,
                "label": scth.labels.addcategory,
                "action": function(obj) {
                    scth.execAddCategoryForNode($node);
                }
            },
            newproduct: {
                "separator_before": false,
                "separator_after": false,
                "label": scth.labels.newproduct,
                "action": function(obj) {
                    scth.execNewProductForNode($node);
                }
            },
            addproduct: {
                "separator_before": false,
                "separator_after": false,
                "label": scth.labels.addproduct,
                "action": function(obj) {
                    scth.execAddProductForNode($node);
                }
            },
            manage: {
                "separator_before": true,
                "separator_after": true,
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
            },
            addcatalog: {
                "separator_before": false,
                "separator_after": false,
                "label": scth.labels.addcatalog,
                "action": function(obj) {
                    scth.execAddCatalog();
                }
            }
        };
    };
    
    var getMenuDefs = function($node) {
        var objectType = getNodeObjectType($node);
        var parentObjectType = getNodeObjectType(getParentNode($node));
        
        var defs = getMenuActionDefs($node);
        var setDefLabel = function(obj, label) {
            if (label) obj.label = label;
            return obj;
        };
        
        var menuDefs = {};
        if (objectType == 'catalog') {
            menuDefs = {
                "edit": setDefLabel(defs.edit, scth.labels.editcatalog),
                "removeassoc": setDefLabel(defs.removeassoc, scth.labels.removefromstore),
                "remove": setDefLabel(defs.remove, scth.labels.deletecatalog),
                "manage": setDefLabel(defs.manage, scth.labels.managecatalog),
                "newcategory": defs.newcategory,
                "addcategory": defs.addcategory,
            };
        } else if (objectType == 'category') {
            menuDefs = {
                "edit": setDefLabel(defs.edit, scth.labels.editcategory),
                "removeassoc": setDefLabel(defs.removeassoc, 
                    (parentObjectType === 'catalog') ? scth.labels.removefromcatalog : scth.labels.removefromcategory),
                "remove": setDefLabel(defs.remove, scth.labels.deletecategory),
                "manage": setDefLabel(defs.manage, scth.labels.managecategory),
                "newcategory": setDefLabel(defs.newcategory, scth.labels.newsubcategory),
                "addcategory": setDefLabel(defs.addcategory, scth.labels.addsubcategory),
                "newproduct": defs.newproduct,
                "addproduct": defs.addproduct
            };
        } else if (objectType == 'product') {
            menuDefs = {
                "edit": setDefLabel(defs.edit, scth.labels.editproduct),
                "removeassoc": setDefLabel(defs.removeassoc, scth.labels.removefromcategory),
                "remove": setDefLabel(defs.remove, scth.labels.deleteproduct),
                "manage": setDefLabel(defs.manage, scth.labels.manageproduct)
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
        var $el = jQuery("#ect-action-menu");
        var menuDefs = getMenuDefs($node);
        
        var useDividers = true;
        var makeDividerItem = function() {
            return scth.markup.getSelMarkup('menuItemDivider', '<li><hr/></li>');
        };
        var lastItemDivider = false;

        $el.empty(); // remove old options
        $.each(menuDefs, function(key, menuDef) {
            if (useDividers && menuDef.separator_before === true) {
                $el.append(makeDividerItem());
            }
        
            var disabled = (menuDef._disabled === true);
            var menuItem = scth.markup.getSelMarkup(disabled ? 'menuItemDisabled' : 'menuItem', '<li><a href=""></a></li>');
            var menuAnchor = menuItem.find('a:last-child');
            if (menuAnchor.length) {
                menuAnchor.attr("href", "javascript:void(0);");
                menuAnchor.text(menuDef.label);
                menuAnchor.click(menuDef.action);
            } else {
                menuItem.click(menuDef.action); // fallback improves markup support
            }
            $el.append(menuItem);
            lastItemDivider = false;
            
            if (useDividers && menuDef.separator_after === true) {
                $el.append(makeDividerItem());
                lastItemDivider = true;
            }
        });
        var postMarkup = scth.markup.getSelMarkup('postMenuItems');
        if (postMarkup) {
            if (useDividers && !lastItemDivider) {
                $el.append(makeDividerItem());
            }
            $el.append(postMarkup);
        }
    };
    
        
    /*
     * Event helpers
     */
    
    this.resolvePreselect = function(targetNodeInfo, noShowFormChange, noShowFormPopulate) {
        // no change implies no populate
        if (noShowFormChange === true) noShowFormPopulate = true;
    
        var prevNoShowFormChange = specFlags.noShowFormChange;
        specFlags.noShowFormChange = noShowFormChange;
    
        var prevNoShowFormPopulate = specFlags.noShowFormPopulate;
        specFlags.noShowFormPopulate = noShowFormPopulate;

        var tree = scth.getJsTree();
        var selected = tree.get_selected();
        
        // have to deselect first or the re-select will not fully work
        tree.deselect_all();
        var activateNode = function($node) {
            tree.select_node($node);
            tree.activate_node($node);
            if (!tree.is_leaf($node)) {
                tree.open_node($node);
            }
        };

        if (selected && selected.length) {
            jQuery.each(selected, function(i, $node) {
                activateNode($node);
                return false; // first only; only support one for now
            });
        } else if (targetNodeInfo && targetNodeInfo.objectIdList && targetNodeInfo.objectIdList.length > 0) {
            var $node = getNodeByObjectIdPath(targetNodeInfo.objectIdList, targetNodeInfo.targetObjectType);
            if ($node) {
                activateNode($node);
            }
        }
        
        specFlags.noShowFormPopulate = prevNoShowFormPopulate;
        specFlags.noShowFormChange = prevNoShowFormChange;
    };
    
    this.bindResolvePreselect = function() {
        var treeElem = jQuery('#'+scth.treeId);
        treeElem.bind('loaded.jstree', function(event, data) {
            scth.resolvePreselect(scth.targetNodeInfo, scth.initialSettings.noShowFormChange === true, scth.initialSettings.noShowFormPopulate === true);
        });
    };
    
    var lastTreeOp = {
        clear: function() {
            this.op = null;
            this.node = null;
            this.node_parent = null;
        },
        isSet: function() {
            return this.node && this.node_parent;
        }
    };
    lastTreeOp.clear();
    
    this.treeCheckCallback = function(op, node, node_parent, node_position, more) {
        if (op === 'copy_node' || op === 'move_node') {
            // IMPORTANT: this dnd check means that the dnd plugin will use the logic below
            // to draw the checkmark icon - however when (more.core === true) we must always
            // return false so that the actual tree move is prevented - we don't want
            // jstree to perform the actual move, because we need to handle ourselves.
            if (more.dnd === true) {
                if (scth.isValidCopyMoveTarget(node, node_parent)) {
                    lastTreeOp.op = op;
                    lastTreeOp.node = node;
                    lastTreeOp.node_parent = node_parent;
                    return true;
                } else {
                    lastTreeOp.clear()
                    return false;
                }
            } else {
                return false;
            }
        }
        return false;
    };
    
    this.bindDnd = function() {
        // NOTE: we use this instead of move_node.jstree or copy_node.jstree, which will not get triggered for us
        jQuery(document).bind('dnd_stop.vakata', function(event, data) {
            if (lastTreeOp.isSet()) {
                //alert('moving node ' + getNodeObjectId(lastTreeOp.node) + ' to parent ' + getNodeObjectId(lastTreeOp.node_parent));
                scth.execCopyMoveAssocForNode(lastTreeOp.node, lastTreeOp.node_parent);
            }
            lastTreeOp.clear();
        });
    };
    
    this.initBindAll = function() {
        scth.bindResolvePreselect();
        scth.bindDnd();
    };
}
    