/* ===========================================================
 * trumbowyg.scipio_links.js v1.0
 * Scipio Links Plugin for trumbowyg
 * ===========================================================
 */

(function ($) {
    'use strict';

    var defaultOptions = {
        getPagesServerPath: '/cms/control/getPages',
        getWebSitesServerPath: '/cms/control/getCmsWebSites',
        data: [],
        statusPropertyName: 'success',
        success: undefined,
        error: undefined,
        macroNames : {
            // WARN: FIXME: currently only cmsPageUrl will respond if you switch the names here...
            // if list, the first listed gets priority and is used to generate new code.
            cmsPageUrl: ['cmsPageUrl', 'pageUrl'], // also valid: pageUrl (but cmsPageUrl is more reusable)
            ofbizUrl: 'ofbizUrl',
            ofbizWebappUrl: 'ofbizWebappUrl',
            ofbizInterWebappUrl: 'ofbizInterWebappUrl',
            ofbizContentUrl: 'ofbizContentUrl',
            ofbizContentAltUrl: 'ofbizContentAltUrl'
        },
        // DEV NOTE: currentWebSiteId may become problematic in the future... for now
        // we are OK to rely on it, but only because UI only supports one website per page.
        // schema supports more and in that case, will not be able to rely on this
        // currentWebSiteId anymore...
        currentWebSiteId: null
    };
    
    // Plugin definition
    $.extend(true, $.trumbowyg, {
        langs: {
            en: {
                scipio_links_title_cmspageurl: 'Cms Page Link',
                scipio_links_title_ofbizurl: 'System Navigation Link',
                scipio_links_title_ofbizcontenturl: 'Content Link',
                scipio_links_insert_auto: 'Link: Auto',
                // DEV NOTE: removed the prefix "Insert " from these because too long and caused wrapping
                scipio_links_insert_cmspageurl: 'Link: Cms Page',
                scipio_links_insert_ofbizurl: 'Link: System Nav', // this is both "Controller Link" and "General Link" - it can do everything, so...
                scipio_links_insert_ofbizcontenturl: 'Link: Content',
                scipio_links_page: 'Page',
                scipio_links_alias_shorthand_cmspageurl: 'alias/shorthand for cmsPageUrl',
                scipio_links_page_designate: 'Page Designation',
                scipio_links_designate_byname: 'By Name',
                scipio_links_designate_byid: 'By ID',
                scipio_links_page_name: 'Page Name',
                scipio_links_page_id: 'Page ID',
                scipio_links_url_parameters: 'URL Parameters',
                scipio_links_uri: 'URI (and params)',
                scipio_links_websiteid: 'Website ID',
                scipio_links_lookupwebsiteid: 'Lookup Website ID',
                scipio_links_fullpath: 'Full Path',
                scipio_links_secure: 'Secure',
                scipio_links_encode: 'Encode',
                scipio_links_escapeas: "Escape As",
                scipio_links_extloginkey: "External Login Key",
                scipio_links_rawparams: "Raw Macro Params",
                scipio_links_strict: "Strict Escaping",
                scipio_links_escapeas: "Escape As",
                scipio_links_variant: 'Variant',
                scipio_links_directive: "Directive",
                scipio_links_interwebapp : "Inter-Webapp",
                scipio_links_abspath : "Path Absolute",
                scipio_links_controller: "Use Controller",
                scipio_links_current_rendering_website: "current rendering website",
                scipio_links_same_as_websiteid: "same as Website ID",
            }
        },
        plugins: {
            scipio_links: {
                init: function (trumbowyg) {
                    trumbowyg.o.plugins.scipio_links = $.extend(true, {}, defaultOptions, trumbowyg.o.plugins.scipio_links || {});
                   
                    var helper = new ScipioCommonTbwHelper(trumbowyg, trumbowyg.o.plugins.scipio_links);
                    helper.currFormInfo.initValues = null; // initial values for the form input fields corresponding to the A link html attributes and the FTL url macro call parameters
                    helper.currFormInfo.inputDefs = null; // the resulting form field input defs that were passed to openModalInsert as field
                    
                    var buildInsertLink = function(t, v, url) {
                        // SCIPIO: this is from trumbowyg core.
                        var link = $(['<a href="', url, '">', v.text, '</a>'].join(''));
                        if (v.title.length > 0) {
                            link.attr('title', v.title);
                        }
                        if (v.target.length > 0) {
                            link.attr('target', v.target);
                        }
                        t.range.deleteContents();
                        t.range.insertNode(link[0]);
                        return true;
                    };
                    
                    // DERIVED FROM trumbowyg createLink function
                    var getCurrentLinkFtlDir = function() {
                        // FIXME: duplication
                        var t = trumbowyg,
                            documentSelection = trumbowyg.doc.getSelection(),
                            node = documentSelection.focusNode;
                        while (['A', 'DIV'].indexOf(node.nodeName) < 0) {
                            node = node.parentNode;
                        }
                        if (node && node.nodeName === 'A') {
                            var $a = $(node);
                            var url = $a.attr('href');
                            return helper.ftlParser.parseDirective(url);
                        }
                        return null;
                    };
                    
                    // HIGH-LEVEL implementation, does everything common to all links
                    // DERIVED FROM trumbowyg createLink function
                    var showFormHandleBuildLinkCommon = function(linkType, refineInputDefsCb, argDefs) {
                        var t = trumbowyg,
                            documentSelection = trumbowyg.doc.getSelection(),
                            node = documentSelection.focusNode,
                            errorMsgs = [];
    
                        while (['A', 'DIV'].indexOf(node.nodeName) < 0) {
                            node = node.parentNode;
                        }
    
                        var url = null;
                        // SCIPIO: holds the initial form input values for the fields corresponding
                        // to the A link html attributes and the FTL url macro call parameters
                        var initValues = {
                            title : null,
                            target : null
                        };
                        
                        if (node && node.nodeName === 'A') {
                            var $a = $(node);
                            url = $a.attr('href');
                            initValues.title = $a.attr('title');
                            initValues.target = $a.attr('target');
                            
                            // SCIPIO: parse the URL to extract the macro args (best-effort) as initial values
                            var ftlDir = helper.ftlParser.parseDirective(url);
                            if (ftlDir.errorMsgName) {
                                var errorMsg = trumbowyg.lang.scipio_common_error_ftl_parsing_error_check + ": ";
                                if (ftlDir.errorMsgHint) {
                                    errorMsg += "[" + ftlDir.errorMsgHint + "] ";
                                }
                                errorMsg += (trumbowyg.lang["scipio_common_error_ftl_" + ftlDir.errorMsgName] || ftlDir.errorMsg);
                                errorMsgs.push(errorMsg);
                            }
                            helper.putFieldInitValues(t, linkType, argDefs, ftlDir, initValues);
                            
                            var range = t.doc.createRange();
                            range.selectNode(node);
                            documentSelection.addRange(range);
                        } else {
                            // SCIPIO: set default initial values
                            helper.putFieldInitValues(t, linkType, argDefs, null, initValues);
                        }

                        t.saveRange();
                        
                        // SCIPIO: transform our argDefs into inputDefs to pass to openModalInsert (instead of raw HTML)
                        var inputDefs = helper.makeFormInputDefs(t, initValues, argDefs);
                        // SCIPIO: this then adds the form field defs for title, target, and text
                        // TODO?: merge the html defs into argDefs somehow with the ftl ones, to get rid of this call...
                        if (typeof refineInputDefsCb === 'function') {
                            inputDefs = refineInputDefsCb(t, inputDefs, initValues, argDefs);
                        }
                        
                        // SCIPIO: save these in global state because some need to be read back later
                        helper.currFormInfo.argDefs = argDefs
                        helper.currFormInfo.initValues = initValues;
                        helper.currFormInfo.inputDefs = inputDefs;
                        
                        var modal = helper.openModalInsert(
                            // Title
                            trumbowyg.lang['scipio_links_title_' + linkType.toLowerCase()],
    
                            // Fields
                            inputDefs,
    
                            // Callback
                            function (v) {
                                var ftlCmd = helper.buildFtlMacroInvoke(helper.getPreferredMacroName(linkType), 
                                        helper.makeCommonFtlInvokeArgDefs(t, v, argDefs));
                                return buildInsertLink(t, v, ftlCmd);
                            }
                        );
                        
                        helper.insertErrorMsgBox(errorMsgs, modal);
                        
                        // SCIPIO: save the modal in global state
                        helper.currFormInfo.modal = modal;
                        return modal;
                    };
            
                    // common macro arg definitions, common to all or most macros, reusable
                    var commonFieldArgDefs = {
                        fullPath: function() {
                            return {
                                inputType: 'select', // NOTE: must be select because ternary value support important
                                inputSelectOptions: helper.commonFieldParts.ternaryBoolSelectOptions('false'),
                                inputValueStrict: true,
                                label: trumbowyg.lang.scipio_links_fullpath,
                                ftlType : 'boolean'
                            };
                        },
                        secure: function() {
                            return {
                                inputType: 'select', // NOTE: must be select because ternary value support important
                                inputSelectOptions: helper.commonFieldParts.ternaryBoolSelectOptions('false'),
                                inputValueStrict: true,
                                label: trumbowyg.lang.scipio_links_secure,
                                ftlType : 'boolean'
                            };
                        },
                        encode: function() {
                            return {
                                inputType: 'select', // NOTE: must be select because ternary value support important
                                inputSelectOptions: helper.commonFieldParts.ternaryBoolSelectOptions('true'),
                                inputValueStrict: true,
                                label: trumbowyg.lang.scipio_links_encode,
                                ftlType : 'boolean',
                                advanced: true
                            };
                        },
                        escapeAs: function() {
                            return {
                                inputType: 'select', 
                                inputSelectOptions: helper.makeOptionArgs([
                                    // NOTE: only support 'html' and 'html-js' because always generating <a elem after
                                    { value: '', text: '-'},
                                    { value: 'html', text: 'html'},
                                    { value: 'html-js', text: 'html-js'}
                                ], ''),
                                inputValueStrict: true,
                                // NOTE: 2017-03-08: using html as default here because it's almost always the best
                                // choice because this is always going inside an <a> link.
                                initValue: "html",
                                label: trumbowyg.lang.scipio_links_escapeas,
                                advanced: true
                            };
                        },
                        webSiteId: function(webSiteData) {
                            return {
                                label: trumbowyg.lang.scipio_links_websiteid,
                                inputType: 'select',
                                inputSelectOptions: helper.makeWebSiteFieldOptions(webSiteData.webSiteList,
                                        {value:'', text: " - (" + trumbowyg.lang.scipio_links_current_rendering_website + ")"}),
                                inputValueStrict: false
                            };
                        },
                        lookupWebSiteId: function(webSiteData) {
                            return {
                                label: trumbowyg.lang.scipio_links_lookupwebsiteid,
                                inputType: 'select',
                                inputSelectOptions: helper.makeWebSiteFieldOptions(webSiteData.webSiteList, 
                                        {value:'', text: " - (" + trumbowyg.lang.scipio_links_same_as_websiteid + ")"}),
                                inputValueStrict: false,
                                advanced: true
                            };
                        },
                        extLoginKey: function() {
                            return {
                                inputType: 'select',
                                inputSelectOptions: helper.commonFieldParts.ternaryBoolSelectOptions('false'),
                                inputValueStrict: true,
                                label: trumbowyg.lang.scipio_links_extloginkey,
                                ftlType : 'boolean'
                            };
                        },
                        rawParams: function() {
                            return {
                                inputType: 'select',
                                inputSelectOptions: helper.commonFieldParts.ternaryBoolSelectOptions(),
                                inputValueStrict: true,
                                label: trumbowyg.lang.scipio_links_rawparams,
                                ftlType : 'boolean',
                                advanced: true
                            };
                        },
                        strict: function() {
                            return {
                                inputType: 'select',
                                inputSelectOptions: helper.commonFieldParts.ternaryBoolSelectOptions(),
                                inputValueStrict: true,
                                label: trumbowyg.lang.scipio_links_strict,
                                ftlType : 'boolean',
                                advanced: true
                            };
                        }
                    };

                    var extendCommonFlagArgDefs = function(argDefs) {
                        return $.extend({}, argDefs, {
                            escapeAs: commonFieldArgDefs.escapeAs(),
                            rawParams: commonFieldArgDefs.rawParams(),
                            strict: commonFieldArgDefs.strict()
                        });
                    };
                    
                    var extendCommonExtFlagArgDefs = function(argDefs) {
                        return $.extend({}, argDefs, {
                            fullPath: commonFieldArgDefs.fullPath(),
                            secure: commonFieldArgDefs.secure(),
                            encode: commonFieldArgDefs.encode(),
                            escapeAs: commonFieldArgDefs.escapeAs(),
                            extLoginKey: commonFieldArgDefs.extLoginKey(),
                            rawParams: commonFieldArgDefs.rawParams(),
                            strict: commonFieldArgDefs.strict()
                        });
                    };
                    
                    var makeCommonHtmlInputDefs = function(t, initValues, argDefs) {
                        // Input fields for the HTML attribute of the A link
                        // TODO?: maybe integrate this into the argDefs, to avoid special cases
                        return {
                            // NOTE: these are from trumbowyg built-in
                            title: {
                                label: t.lang.title,
                                value: initValues.title
                            },
                            text: {
                                // NOTE: this is not set required in trumbowyg createLink
                                label: t.lang.text,
                                value: t.getRangeText()
                            },
                            target: {
                                label: t.lang.target,
                                value: initValues.target
                            }
                        };
                    }
                    
                    var extendHtmlInputDefs = function(t, inputDefs, initValues, argDefs) {
                        return $.extend({}, inputDefs, makeCommonHtmlInputDefs(t, initValues, argDefs));
                    };
                    
                    // BUTTON Definitions
                    var cmsPageUrlHandler = {
                        linkType : "cmsPageUrl",
                        macroNames : helper.getMacroNames("cmsPageUrl"),
                        exec: function () {
                            var linkType = this.linkType;
                            
                            var currPages = {
                                pages : null,
                                getPageName : function(pageId) {
                                    if (this.pages) {
                                        for(var i=0; i < this.pages.length; i++) {
                                            var page = this.pages[i];
                                            if (page.id == pageId) {
                                                return page.name;
                                            }
                                        }
                                    }
                                    return null;
                                },
                                buildHtmlOpts : function(currPageId, currPageName) {
                                    if (typeof currPageId === 'undefined') {
                                        // get from global
                                        currPageId = helper.currFormInfo.initValues.pageId;
                                    }
                                    if (typeof currPageName === 'undefined') {
                                        // NOTE: there is no form field for pageName
                                        currPageName = helper.currFormInfo.initValues.pageName;
                                    }
                                    
                                    var optsHtml = '';
                                    var selectedFound = false;
                                    if (this.pages) {
                                        for(var i=0; i < this.pages.length; i++) {
                                            var page = this.pages[i];
                                            // NOTE: IMPORTANT: the option label must be exactly the page name,
                                            // because we reuse this later... if the page has no name, we'll let
                                            // user worry about it...
                                            optsHtml += '<option value="' + page.id + '"';
                                            if ((currPageId && currPageId === page.id) || (currPageName && currPageName === page.name)) {
                                                optsHtml += ' selected="selected"';
                                                selectedFound = true;
                                            }
                                            optsHtml += ">" + (page.name || page.id) + '</option>';
                                        }
                                    }
                                    // NON-STRICT MODE: if the page wasn't found, make an extra entry with whatever we got
                                    if (!selectedFound && (currPageId || currPageName)) {
                                        optsHtml = '<option value="' + (currPageId || '') + '" selected="selected">'
                                            + (currPageName || currPageId || '')
                                            + '</option>'
                                            + optsHtml;
                                    }
                                    return optsHtml;
                                }
                            };
                            
                            helper.getWebSiteListAndOptions(function(webSiteData) {
                                var modal = showFormHandleBuildLinkCommon(linkType, extendHtmlInputDefs, extendCommonExtFlagArgDefs({
                                    directive: {
                                        inputType: 'select', 
                                        inputSelectOptions: helper.makeUrlMacroNameSelectOptions(linkType, {
                                            pageUrl : "pageUrl (" + trumbowyg.lang.scipio_links_alias_shorthand_cmspageurl + ")"
                                        }),
                                        inputValueStrict: true,
                                        label: trumbowyg.lang.scipio_links_directive,
                                        required: false,
                                        fromFtl: function(k, v, linkType, argDefs, ftlDir, initValues) {
                                            var macroNamesMap = helper.makeUrlMacroNamesObj(true, linkType);
                                            if (macroNamesMap[ftlDir.name] === true) {
                                                initValues.directive = ftlDir.name;
                                            } else {
                                                initValues.directive = helper.getPreferredMacroName(linkType);
                                            }
                                        },
                                        toFtl: function(k, v, args) {
                                            args.macroName = v.value;
                                            return null;
                                        }
                                    },
                                    webSiteId: commonFieldArgDefs.webSiteId(webSiteData),
                                    lookupWebSiteId: commonFieldArgDefs.lookupWebSiteId(webSiteData),
                                    pageId: {
                                        inputType: 'select', 
                                        inputSelectOptions: [], // loaded dynamically
                                        inputValueStrict: false, // allow old now-invalid values, won't hurt the immediate interaction
                                        required: true,
                                        label: trumbowyg.lang.scipio_links_page,
                                        fromFtl: function(k, v, linkType, argDefs, ftlDir, initValues) {
                                            // check if the macro args were specified by ID or by name (if at all there)
                                            var desInitValue = null;
                                            var prevPageName = null;
                                            var prevPageId = null;
                                            // here, arg having actual content is higher prio than just being present
                                            if (ftlDir.args.id) {
                                                desInitValue = "id";
                                                prevPageId = ftlDir.args.id.strValue;
                                            }
                                            if (ftlDir.args.name) {
                                                desInitValue = "name";
                                                prevPageName = ftlDir.args.name.strValue;
                                            }
                                            // higher prio if the arg has actual content
                                            if (prevPageName) { desInitValue = "name"; }
                                            else if (prevPageId) { desInitValue = "id"; }
                                            initValues.pageDesignate = desInitValue;
                                            initValues.pageId = prevPageId;
                                            // NOTE: SPECIAL: pageName does not correspond to a form field... 
                                            // but need to pass this value...
                                            initValues.pageName = prevPageName;
                                        },
                                        toFtl: function(k, v, args) {
                                            if (!v.value || !args.pageDesignate || !args.pageDesignate.value) {
                                                return null; // NOTE: should not happen... but better than crash
                                            }
                                            var des = args.pageDesignate.value;
                                            var pageId = v.value;
                                            if (des == 'name') {
                                                var pageName = null;
                                                if (pageId) {
                                                    pageName = currPages.getPageName(pageId);
                                                }
                                                // TODO: fallback: it's possible we got a removed page by name or ID
                                                // that no longer exists... in this case, we should just
                                                // preserve whatever existed before... right now it is getting wiped...
                                                if (pageName)  {
                                                    return {name:'name', value:helper.makeFtlStrLit(pageName)};
                                                } else if (pageId) {
                                                    // if page had no name, can't honor request
                                                    return {name:'id', value:helper.makeFtlStrLit(pageId)};
                                                } else {
                                                    return null;
                                                }
                                            } else { // 'id'
                                                if (pageId) {
                                                    return {name:'id', value:helper.makeFtlStrLit(pageId)};
                                                } else {
                                                    return null;
                                                }
                                            }
                                        }
                                    },
                                    pageDesignate: {
                                        inputType: 'select', 
                                        inputSelectOptions: [
                                            { value: 'name', text: trumbowyg.lang.scipio_links_designate_byname},
                                            { value: 'id', text: trumbowyg.lang.scipio_links_designate_byid}
                                        ],
                                        inputValueStrict: true,
                                        required: true,
                                        label: trumbowyg.lang.scipio_links_page_designate,
                                        fromFtl: function() { ; }, // do nothing; handled through pageId's fromFtl
                                        toFtl: function() { return null; } // do nothing; handled through pageId's toFtl
                                    },
                                    params: {
                                        // FIXME: 2017-03-08: MAJOR PROBLEM with this field;
                                        // the ampersand (&) AUTOMATICALLY gets translated to &amp; by the editor (or jQuery.html?) when
                                        // the value is inserted back into the markup... can't seem to prevent...
                                        // our URL macros escaping code have a fallback so they _should_ handle it, 
                                        // but it's in effect producing incorrect FTL code and will become a problem at some point...
                                        label: trumbowyg.lang.scipio_links_url_parameters
                                    }
                                }));
                                helper.setWebSiteIdChangeCallback(modal, function(modal) {
                                    // WARN: issues with currentWebSiteId - see definitions
                                    var webSiteId = jQuery(this).val() || trumbowyg.o.plugins.scipio_links.currentWebSiteId;
                                    $.ajax({
                                        url: trumbowyg.o.plugins.scipio_links.getPagesServerPath,
                                        type: 'POST',
                                        data: {'webSiteId': webSiteId},
                                        cache: false,
                                        dataType: 'json',
                                        success: function(data) {
                                            currPages.pages = data.pages;
                                            var optsHtml = currPages.buildHtmlOpts();
                                            jQuery('select[name=pageId]', modal).html(optsHtml);
                                        },
                                        error: trumbowyg.o.plugins.scipio_links.error || function () {
                                            trumbowyg.$c.trigger('tbwscipioerror', [trumbowyg]);
                                        }
                                    });
                                });
                            });
                            return true;
                        }
                    };
                    var cmsPageUrlBtn = {
                        text: trumbowyg.lang.scipio_links_insert_cmspageurl,
                        ico: 'createLink',
                        fn: function () {
                            return cmsPageUrlHandler.exec();
                        }
                    };
                    
                    var ofbizUrlHandler = {
                        linkType : "ofbizUrl",
                        macroNames : ['ofbizUrl', 'ofbizWebappUrl', 'ofbizInterWebappUrl'],
                        exec: function () {
                            var linkType = this.linkType;
                            helper.getWebSiteListAndOptions(function(webSiteData) {
                                var modal = showFormHandleBuildLinkCommon(linkType, extendHtmlInputDefs, extendCommonExtFlagArgDefs({
                                    directive: {
                                        inputType: 'select', 
                                        inputSelectOptions: [
                                            { value: 'ofbizUrl', text: 'ofbizUrl (controller, multi; /website/control/uri)'},
                                            { value: 'ofbizWebappUrl', text: 'ofbizWebappUrl (intra-webapp servlet: /website/customServletUri)'},
                                            { value: 'ofbizInterWebappUrl', text: 'ofbizInterWebappUrl (inter-webapp: /website2/xxx)'}
                                        ],
                                        inputValueStrict: true,
                                        label: trumbowyg.lang.scipio_links_directive,
                                        required: false,
                                        fromFtl: function(k, v, linkType, argDefs, ftlDir, initValues) {
                                            if (ftlDir.name == 'ofbizInterWebappUrl' || ftlDir.name == 'ofbizWebappUrl') {
                                                initValues.directive = ftlDir.name;
                                            } else {
                                                initValues.directive = 'ofbizUrl';
                                            }
                                        },
                                        toFtl: function(k, v, args) {
                                            args.macroName = v.value;
                                            return null;
                                        }
                                    },
                                    webSiteId: commonFieldArgDefs.webSiteId(webSiteData),
                                    uri: {
                                        // FIXME: 2017-03-08: MAJOR PROBLEM with this field;
                                        // the ampersand (&) AUTOMATICALLY gets translated to &amp; by the editor (or jQuery.html?) when
                                        // the value is inserted back into the markup... can't seem to prevent...
                                        // our URL macros escaping code have a fallback so they _should_ handle it, 
                                        // but it's in effect producing incorrect FTL code and will become a problem at some point...
                                        label: trumbowyg.lang.scipio_links_uri,
                                        required: true
                                    },
                                    type: {
                                        inputType: 'select', 
                                        inputSelectOptions: helper.makeOptionArgs([
                                            { value: '', text: ''},
                                            { value: 'intra-webapp', text: 'intra-webapp'},
                                            { value: 'inter-webapp', text: 'inter-webapp'}
                                        ], 'intra-webapp'),
                                        inputValueStrict: true,
                                        label: trumbowyg.lang.scipio_common_type,
                                        required: false,
                                        advanced: true,
                                        fromFtl: function(k, v, linkType, argDefs, ftlDir, initValues) {
                                            if (ftlDir.name == 'ofbizInterWebappUrl') {
                                                initValues.type = 'inter-webapp';
                                            } else if (ftlDir.name == 'ofbizWebappUrl') {
                                                initValues.type = 'intra-webapp';
                                            } else {
                                                helper.fromFtlStd(k, v, linkType, argDefs, ftlDir, initValues);
                                            }
                                        },
                                        toFtl: function(k, v, args) {
                                            if (args.macroName == 'ofbizInterWebappUrl' || args.macroName == 'ofbizWebappUrl') {
                                                return null; // implied
                                            } else {
                                                return helper.toFtlStd(k, v, args);
                                            }
                                        }
                                    },
                                    interWebapp: {
                                        inputType: 'select',
                                        inputSelectOptions: helper.commonFieldParts.ternaryBoolSelectOptions('false'),
                                        inputValueStrict: true,
                                        label: trumbowyg.lang.scipio_links_interwebapp,
                                        ftlType : 'boolean',
                                        advanced: true,
                                        fromFtl: function(k, v, linkType, argDefs, ftlDir, initValues) {
                                            if (ftlDir.name == 'ofbizInterWebappUrl') {
                                                initValues.interWebapp = 'true';
                                            } else if (ftlDir.name == 'ofbizWebappUrl') {
                                                initValues.interWebapp = 'false';
                                            } else {
                                                helper.fromFtlStd(k, v, linkType, argDefs, ftlDir, initValues);
                                            }
                                        },
                                        toFtl: function(k, v, args) {
                                            if (args.macroName == 'ofbizInterWebappUrl' || args.macroName == 'ofbizWebappUrl') {
                                                return null; // implied
                                            } else {
                                                return helper.toFtlStd(k, v, args);
                                            }
                                        }
                                    },
                                    absPath: {
                                        inputType: 'select',
                                        inputSelectOptions: helper.commonFieldParts.ternaryBoolSelectOptions('false'),
                                        inputValueStrict: true,
                                        label: trumbowyg.lang.scipio_links_abspath,
                                        ftlType : 'boolean',
                                        advanced: true,
                                        fromFtl: function(k, v, linkType, argDefs, ftlDir, initValues) {
                                            if (ftlDir.name == 'ofbizWebappUrl') {
                                                initValues.absPath = 'false';
                                            } else {
                                                helper.fromFtlStd(k, v, linkType, argDefs, ftlDir, initValues);
                                            }
                                        },
                                        toFtl: function(k, v, args) {
                                            if (args.macroName == 'ofbizWebappUrl') {
                                                return null; // implied
                                            } else {
                                                return helper.toFtlStd(k, v, args);
                                            }
                                        }
                                    },
                                    controller: {
                                        inputType: 'select',
                                        inputSelectOptions: helper.commonFieldParts.ternaryBoolSelectOptions('true'),
                                        inputValueStrict: true,
                                        label: trumbowyg.lang.scipio_links_controller,
                                        ftlType : 'boolean',
                                        advanced: true,
                                        fromFtl: function(k, v, linkType, argDefs, ftlDir, initValues) {
                                            if (ftlDir.name == 'ofbizWebappUrl') {
                                                initValues.controller = 'false';
                                            } else {
                                                helper.fromFtlStd(k, v, linkType, argDefs, ftlDir, initValues);
                                            }
                                        },
                                        toFtl: function(k, v, args) {
                                            if (args.macroName == 'ofbizWebappUrl') {
                                                return null; // implied
                                            } else {
                                                return helper.toFtlStd(k, v, args);
                                            }
                                        }
                                    }
                                }));
                                
                                var directiveChangeHandler = function(modal, resetValues) {
                                    if (typeof resetValues === 'undefined') {
                                        resetValues = true;
                                    }
                                    var directive = jQuery(this).val();
                                    
                                    var typeElem = jQuery('select[name=type]', modal);
                                    var interWebappElem = jQuery('select[name=interWebapp]', modal);
                                    var absPathElem = jQuery('select[name=absPath]', modal);
                                    var controllerElem = jQuery('select[name=controller]', modal);
                                    
                                    if (resetValues) {
                                        var setValue = function(elem, val) {
                                            elem.val(val);
                                        };
                                        if (directive == 'ofbizInterWebappUrl') {
                                            setValue(typeElem, 'inter-webapp');
                                            setValue(interWebappElem, 'true');
                                            setValue(absPathElem, '');
                                            setValue(controllerElem, '');
                                        } else if (directive == 'ofbizWebappUrl') {
                                            setValue(typeElem, 'intra-webapp');
                                            setValue(interWebappElem, 'false');
                                            setValue(absPathElem, 'false');
                                            setValue(controllerElem, 'false');
                                        } else {
                                            setValue(typeElem, '');
                                            setValue(interWebappElem, '');
                                            setValue(absPathElem, '');
                                            setValue(controllerElem, '');
                                        }
                                    }
                                    
                                    var setDisabled = function(elem, val) {
                                        elem.prop('disabled', val);
                                        elem.removeClass('disabled');
                                        if (val) {
                                            elem.addClass('disabled');
                                        }
                                    };
                                    if (directive == 'ofbizInterWebappUrl') {
                                        setDisabled(typeElem, true);
                                        setDisabled(interWebappElem, true);
                                        setDisabled(absPathElem, false);
                                        setDisabled(controllerElem, false);
                                    } else if (directive == 'ofbizWebappUrl') {
                                        setDisabled(typeElem, true);
                                        setDisabled(interWebappElem, true);
                                        setDisabled(absPathElem, true);
                                        setDisabled(controllerElem, true);
                                    } else {
                                        setDisabled(typeElem, false);
                                        setDisabled(interWebappElem, false);
                                        setDisabled(absPathElem, false);
                                        setDisabled(controllerElem, false);
                                    }
                                };
                                var select = helper.setSelectChangeCallback('directive', modal, directiveChangeHandler, false);
                                directiveChangeHandler.call(select, modal, false); // do NOT reset values on first load (disable only)
                            });
                            return true;
                        }
                    };
                    var ofbizUrlBtn = {
                        text: trumbowyg.lang.scipio_links_insert_ofbizurl,
                        ico: 'createLink',
                        fn: function () {
                            return ofbizUrlHandler.exec();
                        }
                    };
                    
                    var ofbizContentUrlHandler = {
                        linkType : "ofbizContentUrl",
                        macroNames : ['ofbizContentUrl'], // 'ofbizContentAltUrl'
                        exec: function () {
                            var linkType = this.linkType;
                            helper.getWebSiteListAndOptions(function(webSiteData) {
                                showFormHandleBuildLinkCommon(linkType, extendHtmlInputDefs, extendCommonFlagArgDefs({
                                    directive: {
                                        inputType: 'select', 
                                        inputSelectOptions: [
                                            { value: 'ofbizContentUrl', text: 'ofbizContentUrl'},
                                            // TODO: the fields are different for alt (uses contentId), not yet supported...
                                            //{ value: 'ofbizContentAltUrl', text: 'ofbizContentAltUrl'},
                                        ],
                                        inputValueStrict: true,
                                        label: trumbowyg.lang.scipio_links_directive,
                                        required: false,
                                        fromFtl: function(k, v, linkType, argDefs, ftlDir, initValues) {
                                            // TODO: ofbizContentAltUrl
                                            //if (ftlDir.name == 'ofbizContentAltUrl') {
                                            //    initValues.directive = ftlDir.name;
                                            //} else {
                                            initValues.directive = 'ofbizContentUrl';
                                            //}
                                        },
                                        toFtl: function(k, v, args) {
                                            args.macroName = v.value;
                                            return null;
                                        }
                                    },
                                    uri: {
                                        // FIXME: 2017-03-08: MAJOR PROBLEM with this field;
                                        // the ampersand (&) AUTOMATICALLY gets translated to &amp; by the editor (or jQuery.html?) when
                                        // the value is inserted back into the markup... can't seem to prevent...
                                        // our URL macros escaping code have a fallback so they _should_ handle it, 
                                        // but it's in effect producing incorrect FTL code and will become a problem at some point...
                                        label: trumbowyg.lang.scipio_links_uri,
                                        required: true
                                    },
                                    variant: {
                                        label: trumbowyg.lang.scipio_links_variant
                                    }
                                }));
                            });
                            return true;
                        }
                    };
                    var ofbizContentUrlBtn = {
                        text: trumbowyg.lang.scipio_links_insert_ofbizcontenturl,
                        ico: 'createLink',
                        fn: function () {
                            return ofbizContentUrlHandler.exec();
                        }
                    };
                    
                    var handlerList = [cmsPageUrlHandler, ofbizUrlHandler, ofbizContentUrlHandler];
                    
                    // SCIPIO: this is a special function that will attempt to identify the right
                    // dialog to show for an existing link. if it is not a CMS or ofbiz URL,
                    // it shows the regular createLink instead.
                    // DEV NOTE: this cannot completely substitute for the stock createLink dialog,
                    // because some users may need/wnat to edit cmsPageUrl and ofbizUrl using the
                    // simpler form instead of our helper forms.
                    var autoUrlHandler = {
                        exec: function () {
                            var ftlDir = getCurrentLinkFtlDir(); // FIXME?: this call cause the link to be parsed twice
                            if (ftlDir && ftlDir.type == "macro" && ftlDir.name) {
                                for(var i=0; i < handlerList.length; i++) {
                                    if (jQuery.inArray(ftlDir.name, handlerList[i].macroNames) > -1) {
                                        return handlerList[i].exec();
                                    }
                                }
                            }
                            return trumbowyg.createLink();
                        }
                    };
                    var autoUrlBtn = {
                        text: trumbowyg.lang.scipio_links_insert_auto,
                        ico: 'createLink',
                        fn: function () {
                            return autoUrlHandler.exec();
                        }
                    };

                    trumbowyg.addBtnDef('scipio_links_cmspageurl', cmsPageUrlBtn);
                    trumbowyg.addBtnDef('scipio_links_ofbizurl', ofbizUrlBtn);
                    trumbowyg.addBtnDef('scipio_links_ofbizcontenturl', ofbizContentUrlBtn);
                    trumbowyg.addBtnDef('scipio_links_autourl', autoUrlBtn);
                }
            }
        }
    });
})(jQuery);
