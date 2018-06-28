/* ===========================================================
 * trumbowyg.scipio_assets.js v1.0
 * Scipio Assets & Menus Plugin for trumbowyg
 * ===========================================================
 */

(function ($) {
    'use strict';

    var defaultOptions = {
        getAssetTypesServerPath: '/cms/control/getAssetTypes', // NOTE: caller must override
        getAssetsServerPath: '/cms/control/getAssets', // NOTE: caller must override
        getAssetAttributesServerPath: '/cms/control/getAssetAttributes', // NOTE: caller must override
        getWebSitesServerPath: '/cms/control/getCmsWebSites', // NOTE: caller must override
        
        data: [],
        statusPropertyName: 'success',
        success: undefined,
        error: undefined,
        macroNames: {
            asset: ['cmsAsset', 'asset'] // the first in list is the one used to generate
        }
    };

    // Plugin definition
    $.extend(true, $.trumbowyg, {
        langs: {
            en: {
                scipio_assets_asset: 'Asset',
                scipio_assets_websiteid: 'Website ID',
                scipio_assets_title: 'Assets and Menus',
                scipio_assets_insert: 'Insert Asset Or Menu',
                scipio_assets_asset_designate: 'Asset Designation',
                scipio_assets_designate_byname: 'By Name',
                scipio_assets_designate_byid: 'By ID',
                scipio_assets_asset_attributes: 'Asset Attributes'
            }
        },
        plugins: {
            scipio_assets: {
                init: function (trumbowyg) {
                    trumbowyg.o.plugins.scipio_assets = $.extend(true, {}, defaultOptions, trumbowyg.o.plugins.scipio_assets || {});

                    var helper = new ScipioCommonTbwHelper(trumbowyg, trumbowyg.o.plugins.scipio_assets);
                    var cmsAttrPrefix = "cmsAttrib_";
                    
                    var currAssets = {
                        assets : null,
                        getAssetName : function(assetId) {
                            if (this.assets) {
                                for(var i=0; i < this.assets.length; i++) {
                                    var asset = this.assets[i];
                                    if (asset.assetTemplateId == assetId) {
                                        return asset.templateName;
                                    }
                                }
                            }
                            return null;
                        },
                        buildHtmlOpts : function(currAssetId, currAssetName) {
                            if (typeof currAssetId === 'undefined') {
                                // get from global
                                currAssetId = helper.currFormInfo.initValues.assetId;
                            }
                            if (typeof currAssetName === 'undefined') {
                                // NOTE: there is no form field for assetName
                                currAssetName = helper.currFormInfo.initValues.assetName;
                            }
                            
                            var optsHtml = '';
                            var selectedFound = false;
                            if (this.assets) {
                                for(var i=0; i < this.assets.length; i++) {
                                    var asset = this.assets[i];
                                    // NOTE: IMPORTANT: the option label must be exactly the asset name,
                                    // because we reuse this later... if the asset has no name, we'll let
                                    // user worry about it...
                                    optsHtml += '<option value="' + asset.assetTemplateId + '"';
                                    if ((currAssetId && currAssetId === asset.assetTemplateId) || (currAssetName && currAssetName === asset.templateName)) {
                                        optsHtml += ' selected="selected"';
                                        selectedFound = true;
                                    }
                                    optsHtml += ">" + (asset.templateName || asset.assetTemplateId) + '</option>';
                                }
                            }
                            // NON-STRICT MODE: if the asset wasn't found, make an extra entry with whatever we got
                            if (!selectedFound && (currAssetId || currAssetName)) {
                                optsHtml = '<option value="' + (currAssetId || '') + '" selected="selected">'
                                    + (currAssetName || currAssetId || '')
                                    + '</option>'
                                    + optsHtml;
                            }
                            return optsHtml;
                        }
                    };
                    
                    var setContentTypeIdChangeCallback = function(modal, cb, immediate) {
                        var select = jQuery('select[name=contentTypeId]', modal);
                        if (immediate !== false) {
                            cb.call(select, modal);
                        }
                        select.on('change', function() {
                            cb.call(this, modal);
                        });
                    };
                    
                    var buildInputsForAttributes = function(attributes) {
                        var res = '';
                        if (attributes && attributes.length) {
                            for(var i = 0; i < attributes.length; i++) {
                                var attr = attributes[i];
                                
                                var fieldName = cmsAttrPrefix + attr.attributeName;
                                var fieldLabel = attr.attributeName;
                                
                                var desc = '';
                                if (attr.displayName) {
                                    desc = helper.cleanFieldValue(attr.displayName);
                                }
                                if (attr.inputHelp) {
                                    desc += (desc ? ': ' : '') + helper.cleanFieldValue(attr.inputHelp);
                                }
                                if (attr.inputType) {
                                    var str = trumbowyg.lang.scipio_common_type + ': ' + attr.inputType;
                                    if (desc) {
                                        desc += ' (' + str + ')';
                                    } else {
                                        desc = str;
                                    }
                                }
                                if (attr.defaultValue) {
                                    var str = trumbowyg.lang.scipio_common_default + ': ' + helper.cleanFieldValue(attr.defaultValue);
                                    if (desc) {
                                        desc += ' (' + str + ')';
                                    } else {
                                        desc = str;
                                    }
                                }
                                // TODO: we currently can't enforce the 'required' from here...
                                if (attr.required === 'Y') {
                                    var str = trumbowyg.lang.scipio_common_required;
                                    if (desc) {
                                        desc += ' (' + str + ')';
                                    } else {
                                        desc = str;
                                    }
                                }
                                if (attr.expandLang) {
                                    // TODO: better description of this
                                    var str = 'expandLang: ' + helper.cleanFieldValue(attr.expandLang);
                                    if (desc) {
                                        desc += ' (' + str + ')';
                                    } else {
                                        desc = str;
                                    }
                                }
                                if (attr.targetType) {
                                    var str = 'targetType: ' + helper.cleanFieldValue(attr.targetType);
                                    if (desc) {
                                        desc += ' (' + str + ')';
                                    } else {
                                        desc = str;
                                    }
                                }
                                
                                // NOTE: only simple boolean if no expandLang is set, otherwise complex expression needed
                                var isSimpleBoolean = (attr.inputType == "BOOLEAN" && (!attr.expandLang || attr.expandLang === 'NONE'));
                                
                                var fieldAttr = {'scipio-ftl-type':isSimpleBoolean ? 'boolean' : 'string', // TODO: other type support
                                     // FIXME?: cleaner tooltip? (browser compat)
                                    'data-tip':desc, 'title':desc, 'alt':desc
                                };
                                
                                // TODO: ternary boolean (only if non-expanding)
                                if (isSimpleBoolean) {
                                    res += helper.buildSelectFieldHtml(fieldName, null, fieldLabel, fieldAttr, true,
                                        [
                                            {'value':'', 'text':''},
                                            {'value':'true', 'text':'true' + (attr.defaultValue === 'true' ? ' (' + trumbowyg.lang.scipio_common_default + ')' : '')},
                                            {'value':'false', 'text':'false' + (attr.defaultValue === 'false' ? ' (' + trumbowyg.lang.scipio_common_default + ')' : '')},
                                        ]);
                                } else {
                                    res += helper.buildInputFieldHtml('text', fieldName, null, fieldLabel, fieldAttr);
                                }
                            }
                        }
                        return res;
                    };
                    
                    var assetChangeHandler = function(modal) {
                        var attrCntr = jQuery('#trumbowyg-scp-asset-attribs-ctnr', modal);
                        attrCntr.empty(); // remove all so don't show invalid fields if error happens
                        
                        var assetId = jQuery('select[name=assetId]', modal).val();
                        if (!assetId) {
                            return;
                        }
                        $.ajax({
                            url: trumbowyg.o.plugins.scipio_assets.getAssetAttributesServerPath,
                            type: 'POST',
                            data: {"assetTemplateId":assetId},
                            cache: false,
                            dataType: 'json',
                            success: function(data) {
                                var inputHtml = buildInputsForAttributes(data.attributeValues);
                                attrCntr.html(inputHtml);
                            },
                            error: trumbowyg.o.plugins.scipio_assets.error || function () {
                                trumbowyg.$c.trigger('tbwscipioerror', [trumbowyg]);
                            }
                        });
                    };
                    
                    var getShowAssets = function(modal) {
                        var webSiteId = jQuery('select[name=webSiteId]', modal).val();
                        var contentTypeId = jQuery('select[name=contentTypeId]', modal).val();
                        var params = {};
                        if (webSiteId) {
                            params.webSiteId = webSiteId;
                        }
                        if (contentTypeId) {
                            params.contentTypeId = contentTypeId;
                        }
                        $.ajax({
                            url: trumbowyg.o.plugins.scipio_assets.getAssetsServerPath,
                            type: 'POST',
                            data: params,
                            cache: false,
                            dataType: 'json',
                            success: function(data) {
                                currAssets.assets = data.assetTemplateValues;
                                var optsHtml = currAssets.buildHtmlOpts();
                                jQuery('select[name=assetId]', modal).html(optsHtml);
                                assetChangeHandler(modal);
                            },
                            error: trumbowyg.o.plugins.scipio_assets.error || function () {
                                trumbowyg.$c.trigger('tbwscipioerror', [trumbowyg]);
                            }
                        });
                    };
                    
                    var showFormHandleBuildInvoke = function(argDefs) {
                        var t = trumbowyg,
                            documentSelection = trumbowyg.doc.getSelection(),
                            node = documentSelection.focusNode,
                            errorMsgs = [];
    
                        var initValues = {};
                        
                        // SCIPIO: set default initial values
                        helper.putFieldInitValues(t, null, argDefs, null, initValues);

                        t.saveRange();
                        
                        // SCIPIO: transform our argDefs into inputDefs to pass to openModalInsert (instead of raw HTML)
                        var inputDefs = helper.makeFormInputDefs(t, initValues, argDefs);
                        
                        // SCIPIO: save these in global state because some need to be read back later
                        helper.currFormInfo.argDefs = argDefs
                        helper.currFormInfo.initValues = initValues;
                        helper.currFormInfo.inputDefs = inputDefs;
                        
                        var modal = helper.openModalInsert(
                            // Title
                            trumbowyg.lang.scipio_assets_title,
    
                            // Fields
                            inputDefs,
    
                            // Callback
                            function (v) {
                                var fields = jQuery(':input', jQuery('#trumbowyg-scp-asset-attribs-ctnr', modal));
                                var extraFtlArgs = helper.makeExtraFtlInvokeArgDefs(fields, 'cmsAttrib');
                                var ftlArgs = helper.makeCommonFtlInvokeArgDefs(t, v, argDefs);
                                ftlArgs = jQuery.extend({}, extraFtlArgs, ftlArgs);
                                ftlArgs.def = {value: "global"}; // always global here
                                var ftlCmd = helper.buildFtlMacroInvoke(helper.getPreferredMacroName("asset"), ftlArgs);
                                trumbowyg.execCmd('insertHTML', ftlCmd);
                                return true;
                            }
                        );
                        
                        helper.insertErrorMsgBox(errorMsgs, modal);
                        
                        // SCIPIO: save the modal in global state
                        helper.currFormInfo.modal = modal;
                        return modal;
                    };
                    
                    // BUTTON Definitions
                    var scipioAssetsBtn = {
                        title: trumbowyg.lang.scipio_assets_insert,
                        text: '<i class="fa fa-file-code-o" style="margin-left:5px; margin-right:5px;"/>',
                        hasIcon: false,
                        fn: function () {
                            $.ajax({
                                url: trumbowyg.o.plugins.scipio_assets.getAssetTypesServerPath,
                                type: 'POST',
                                data: {'deep':true},
                                cache: false,
                                dataType: 'json',
                                success: function(assetTypeData) {
                                    helper.getWebSiteListAndOptions(function(webSiteData) {
                                        var modal = showFormHandleBuildInvoke({
                                            // TODO: field defs and dialogs to allow ctxVars, attribs, and ovrdCtxVars
                                            // these are MAPS and the FTL parser does not currently support them...
                                            // users have to add manually to the call...
                                            contentTypeId: {
                                                label: trumbowyg.lang.scipio_common_type,
                                                inputType: 'select',
                                                inputSelectOptions: helper.makeContentTypeOptions(assetTypeData.contentTypeValues,
                                                        {value:'', text: " - "}),
                                                inputValueStrict: false,
                                                toFtl: function() { ; }, // do nothing
                                                fromFtl: function() { ; } // do nothing
                                            },
                                            webSiteId: {
                                                label: trumbowyg.lang.scipio_assets_websiteid,
                                                inputType: 'select',
                                                inputSelectOptions: helper.makeWebSiteFieldOptions(webSiteData.webSiteList,
                                                        {value:'', text: " - "}),
                                                inputValueStrict: false
                                            },
                                            assetId: {
                                                inputType: 'select', 
                                                inputSelectOptions: [], // loaded dynamically
                                                inputValueStrict: false, // allow old now-invalid values, won't hurt the immediate interaction
                                                required: true,
                                                label: trumbowyg.lang.scipio_assets_asset,
                                                fromFtl: function() { ; }, // nothing to do
                                                toFtl: function(k, v, args) {
                                                    if (!v.value || !args.assetDesignate || !args.assetDesignate.value) {
                                                        return null; // NOTE: should not happen... but better than crash
                                                    }
                                                    var des = args.assetDesignate.value;
                                                    var assetId = v.value;
                                                    if (des == 'name') {
                                                        var assetName = null;
                                                        if (assetId) {
                                                            assetName = currAssets.getAssetName(assetId);
                                                        }
                                                        // TODO: fallback: it's possible we got a removed asset by name or ID
                                                        // that no longer exists... in this case, we should just
                                                        // preserve whatever existed before... right now it is getting wiped...
                                                        if (assetName)  {
                                                            return {name:'name', value:helper.makeFtlStrLit(assetName)};
                                                        } else if (assetId) {
                                                            // if asset had no name, can't honor request
                                                            return {name:'id', value:helper.makeFtlStrLit(assetId)};
                                                        } else {
                                                            return null;
                                                        }
                                                    } else { // 'id'
                                                        if (assetId) {
                                                            return {name:'id', value:helper.makeFtlStrLit(assetId)};
                                                        } else {
                                                            return null;
                                                        }
                                                    }
                                                }
                                            },
                                            assetDesignate: {
                                                inputType: 'select', 
                                                inputSelectOptions: [
                                                    { value: 'name', text: trumbowyg.lang.scipio_assets_designate_byname},
                                                    { value: 'id', text: trumbowyg.lang.scipio_assets_designate_byid}
                                                ],
                                                inputValueStrict: true,
                                                required: true,
                                                label: trumbowyg.lang.scipio_assets_asset_designate,
                                                fromFtl: function() { ; }, // do nothing
                                                toFtl: function() { return null; } // do nothing; handled through assetId toFtl
                                            },
                                            assetAttribsContainer: { // virtual field
                                                inputType: 'manual',
                                                inputMarkup: '<div class="trumbowyg-scp-asset-attribs-section">' 
                                                    + '<h4 class="trumbowyg-scp-field-section-title">' + trumbowyg.lang.scipio_assets_asset_attributes + '</h4>'
                                                    + '<div id="trumbowyg-scp-asset-attribs-ctnr" class="trumbowyg-scp-asset-attribs">'
                                                    // FIXME: this is disgusting hack that prevents a bug with the trumbowyg modal fix/scrolling that otherwise 
                                                    // makes the forms unusable... these dummy fields get removed immediately by javascript
                                                    + helper.makeDummyInputFields("Dummy", 100)
                                                    + "</div>"
                                                    + "</div>",
                                                toFtl: function(k, v, args) {
                                                    var attribs = {};
                                                    jQuery.each(args, function(argName, argInfo) {
                                                        if (helper.stringStartsWith(argName, cmsAttrPrefix)) {
                                                            argName = argName.substring(cmsAttrPrefix.length);
                                                            var value = argInfo.value;
                                                            if (typeof value !== 'undefined' && value != null 
                                                                    && !(jQuery.type(value)==='string' && value.length <= 0)) {
                                                                attribs[argName] = {'type':argInfo.type, 'value':value};
                                                            }
                                                        }
                                                    });
                                                    if (!jQuery.isEmptyObject(attribs)) {
                                                        var attribStr = helper.ftlParser.toArgsMapStrRepr(attribs);
                                                        return {'name':'attribs', 'value':attribStr};
                                                    }
                                                    return null;
                                                },
                                                fromFtl: function() { ; } // do nothing
                                            },
                                        });
                                        
                                        getShowAssets(modal);
                                        helper.setWebSiteIdChangeCallback(modal, getShowAssets, false);
                                        setContentTypeIdChangeCallback(modal, getShowAssets, false);
                                        helper.setSelectChangeCallback('assetId', modal, assetChangeHandler, false);
                                    });
                                },
                                error: trumbowyg.o.plugins.scipio_assets.error || function () {
                                    trumbowyg.$c.trigger('tbwscipioerror', [trumbowyg]);
                                }
                            });
                            return true;
                        }
                    };
                    
                    trumbowyg.addBtnDef('scipio_assets', scipioAssetsBtn);
                }
            }
        }
    });
})(jQuery);
