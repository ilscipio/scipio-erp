/* ===========================================================
 * trumbowyg.scipio_common.js v1.0
 * Scipio Common Code for Plugins for trumbowyg
 * ===========================================================
 */


if (typeof ScipioFtlParser === 'undefined') {

    /**
     * Home-brewed, dirty FTL parser implementation to extract values from FTL commands.
     * WARN: MAKESHIFT FTL PARSER
     * FIXME: ONLY HANDLES BRACKET SYNTAX
     * FIXME: ONLY HANDLES STRING AND BOOLEAN ARGS, ANYTHING ELSE CAUSES ABORT WITH complete=false.
     * TODO?: MOVE ELSEWHERE
     */
    function ScipioFtlParser() {
        
        this.newDirectiveInfo = function() {
            return { type:null, name:null, args:{}, nested:null, nesting:false, 
                complete:false, errorMsg:null, errorMsgName:null, errorMsgHint:null};
        };
        
        this.parseDirective = function(str) {
            var dir = this.newDirectiveInfo();
            str = str || '';
            var paramStr;
            // check for non-nesting macro
            var nameSplit = str.match(/^\s*\[@([a-zA-Z0-9_]+)(\s*(.*?))?[/]\]\s*$/);
            if (nameSplit) {
                dir.type = "macro";
                dir.name = nameSplit[1];
                paramStr = nameSplit[2];
            } else {
                // check for macro with nested value
                nameSplit = str.match(/^\s*\[@([a-zA-Z0-9_]+)([\s\]].*)\[[/]@(\1)?\]\s*$/)
                if (nameSplit) {
                    dir.type = "macro";
                    dir.name = nameSplit[1];
                    paramStr = nameSplit[2];
                    dir.nesting = true;
                }
            }
            if (!dir.name) {
                dir.errorMsg = "Unsupported freemarker directive or syntax (only macro supported; non-nesting syntax more reliable)";
                return dir;
            }
            
            var startsWith = function(str, substr) {
                return (str.indexOf(substr) == 0);
            };
            
            // BEST-EFFORT parse macro arguments
            var hasMore = true;
            var m;
            var str = paramStr;
            while(hasMore) {
                // PARAM NAME
                m = str.match(/^\s*([a-zA-Z0-9_]+)=\s*(.*?)$/)
                if (m) {
                    var argName = m[1];
                    str = m[2] || '';
                    if (!str) {
                        // PARSING ERROR; ABORT AND RETURN BEST-EFFORT
                        dir.errorMsgName = "missing_macro_argument_value";
                        dir.errorMsgHint = "arg: " + argName;
                        dir.errorMsg = "syntax error: missing macro argument value";
                        return dir;
                    }
                    // STRING
                    m = str.match(/^(r?(['"])).*$/);
                    if (m) {
                        var rawStr = (m[1].length >= 2);
                        var delim = m[2];
                        // tries to honor backslash escaping...
                        var endPos = this.indexOfClosingStringLit(str, 0, delim, rawStr);
                        if (endPos != -1) {
                            var strLit = str.substring(0, endPos + 1);
                            var strValue = this.unescapeStringLit(str.substring(rawStr ? 2 : 1, endPos));
                            dir.args[argName] = {value: strValue, strValue: strValue, strLit: strLit, rawStr: rawStr};
                            str = str.substring(endPos + 1);
                        } else {
                            // PARSING ERROR; ABORT AND RETURN BEST-EFFORT
                            dir.errorMsgName = "unterm_string_literal";
                            dir.errorMsgHint = "arg: " + argName + " (string)";
                            dir.errorMsg = "syntax error: unterminated string literal in macro string argument";
                            return dir;
                        }
                    } else {
                        // BOOLEAN
                        m = str.match(/^(true|false)([\s/\]].*)?$/); // TODO: double-check the possible delimiters (space, frontslash, bracket...?)
                        if (m) {
                            var strValue = m[1];
                            dir.args[argName] = {value: (strValue == 'true'), strValue: strValue, strLit: m[1]};
                            str = m[2] || '';
                        } else {
                            // NUMBER
                            m = str.match(/^(-?[0-9.])([\s/\]].*)?$/);
                            if (m) {
                                var strValue = m[1];
                                dir.args[argName] = {value: (strValue.indexOf(".") != -1) ? parseFloat(strValue) : parseInt(strValue), 
                                        strValue: strValue, strLit: m[1]};
                                str = m[2] || '';
                            } else {
                                // FIXME: CAN'T HANDLE ANYTHING EXCEPT STRING OR BOOLEAN; ABORT AND RETURN BEST-EFFORT
                                dir.errorMsgName = "unsup_directive_arg_syntax";
                                dir.errorMsgHint = "arg: " + argName;
                                dir.errorMsg = "unsupported directive/macro argument type or syntax"
                                    + " (only string literal, number, boolean supported;" 
                                    + " no parenthesis, variable, function or built-in support unless interpolated in string)";
                                return dir;
                            }
                        }
                    }
                } else {
                    hasMore = false;
                }
            }
            if (dir.nesting) {
                m = str.match(/^\s*](.*?)$/)
                if (m) {
                    dir.nested = m[1];
                }
            }
            dir.complete = true;
            return dir;
        };
         
        /**
         * Returns the index of the closing string literal, or -1 if not found (means syntax error),
         * trying to honor escaping rules.
         * startingIndex should point to the opening delim (', ", r", r'), is 0 by default.
         * delim and rawStr can be passed as optimization.
         */
        this.indexOfClosingStringLit = function(str, startingIndex, delim, rawStr) {
            if (typeof startingIndex !== 'number') {
                startingIndex = 0; 
            }
            if (!str || (str.length - startingIndex) < 2) {
                return -1;
            }
            
            if (!delim) {
                var c = str.charAt(startingIndex);
                if (c === "'" || c === '"') {
                    delim = c;
                    rawStr = false;
                } else if (first === "r") {
                    c = str.charAt(startingIndex + 1);
                    if (c === "'" || c === '"') {
                        delim = c;
                        rawStr = true;
                    } else {
                        return -1;
                    }
                } else {
                    return -1;
                }
            }

            if (rawStr) {
                return str.indexOf(delim, startingIndex + 2);
            }
            
            var numSlash = 0;
            for(var i = startingIndex + 1, len = str.length; i < len; i++) {
                var c = str.charAt(i);
                if (c == delim) {
                    // only if even number of slashes immediately before it
                    if ((numSlash % 2) == 0) {
                        return i;
                    } else {
                        numSlash=0;
                    }
                } else if (c == '\\') {
                    numSlash++;
                } else {
                    numSlash=0;
                }
            }
            return -1;
        }
        
        this.replaceAll = function(target, search, replacement) {
            return target.split(search).join(replacement);
        };
         
        this.unescapeStringLit = function(str, delim) {
            return this.replaceAll(this.replaceAll(str, '\\\\', '\\'), '\\'+delim, delim);
        };
    
        this.escapeStringLit = function(str, delim) {
            return this.replaceAll(this.replaceAll(str, '\\\\', '\\'), delim, '\\'+delim);
        };
         
        // TODO: support double quotes
        this.toArgsMapStrRepr = function(args) {
            var str = '';
            var self = this;
            jQuery.each(args, function(name, info) {
                if (str) str += ', ';
                str += "'" + name + "':" + self.toArgStrRepr(info.type, info.value);
            });
            return '{' + str + "}";
        };
        
        // TODO: support double quotes
        // TODO: map & list support
        this.toArgStrRepr = function(type, value) {
            if (type === 'boolean') {
                if (value === 'true' || value === true) {
                    return 'true';
                } else if (val === 'false' || value === false) {
                    return 'false';
                } else {
                    return "''";
                }
            } else {
                if (typeof value === 'undefined' || value == null) {
                    value = '';
                } else {
                    value = '' + value; // to string
                }
                return "'" + this.escapeStringLit(value, "'") + "'";
            }
        };
        
        this._parseDirectiveTest = function() {
            alert(JSON.stringify(this.parseDirective("[@pageUrl name='thename' webSiteId='theWebSite' fullPath=true someNumber=0/]")));
            alert(JSON.stringify(this.parseDirective("[@ofbizUrl webSiteId='theWebSite' ]myUri[/@ofbizUrl]")));
        };
    };
}

(function ($) {
    $.extend(true, $.trumbowyg, {
        langs: {
            en: {
                scipio_common_advanced : "Advanced",
                scipio_common_default : "Default",
                scipio_common_required : "Required",
                scipio_common_type : "Type",
                scipio_common_error_ftl_parsing_error: "Freemarker parsing error",
                scipio_common_error_ftl_parsing_error_check: "Freemarker parsing error (please check raw source)",
                scipio_common_error_ftl_missing_macro_argument_value: "syntax error: missing macro argument value",
                scipio_common_error_ftl_unterm_string_literal: "syntax error: unterminated string literal in macro string argument",
                scipio_common_error_ftl_unsup_directive_arg_syntax: "unsupported directive/macro argument type or syntax"
                    + " (only string literal, number, boolean supported;" 
                    + " no parenthesis, variable, function or built-in support unless interpolated in string)",
            }
        }
    });
})(jQuery);


/**
 * Scipio trumbowyg plugin common code/helper.
 */
function ScipioCommonTbwHelper(trumbowyg, pluginObj) { // pluginObj is e.g.: trumbowyg.o.plugins.scipio_links
    var helper = this;
    var ftlParser = new ScipioFtlParser();
    this.ftlParser = ftlParser;
    
    // SPECIAL: holds the last modal and other form-related info so it can be read back from callbacks more easily (global state)
    this.currFormInfo = {
        modal : null, // the modal jQuery object
        argDefs : null // high-level arg defs, our custom format
    };

    this.stringStartsWith = function(str, prefix) {
        return (str.indexOf(prefix) == 0);
    };
    
    this.makeUrlMacroNamesObj = function(initValue, linkType) { // makes an object where keys are all our macro names
        var obj = {};
        if (linkType) {
            jQuery.each(helper.getMacroNames(linkType), function(i, e) {
                obj[e] = initValue || {};
            });
        } else {
            var macroNames = pluginObj.macroNames;
            jQuery.each(macroNames, function(k, v) {
                jQuery.each(helper.getMacroNames(k), function(i, e) {
                   obj[e] = initValue || {};
                });
            });
        }
        return obj;
    };
    
    this.makeUrlMacroNameSelectOptions = function(linkType, texts) {
        if (!texts) {
            texts = {};
        }
        var macroNames = this.getMacroNames(linkType);
        var opts = [];
        jQuery.each(macroNames, function(i, e) {
            opts.push({'value':e, 'text':texts[e] || e});
        });
        return opts;
    };
    
    this.getMacroNames = function(linkType) {
        var macroNames = pluginObj.macroNames[linkType];
        if (macroNames) {
            if($.type(macroNames) === "string") {
                return [macroNames];
            } else {
                return macroNames;
            }
        }
        return null;
    };
    
    this.getPreferredMacroName = function(linkType) {
        var macroNames = this.getMacroNames(linkType);
        if (macroNames) {
            return macroNames[0];
        }
        return null;
    };
    
    /* instead of custom HTML, going to build form manually using own version of openModalInsert below
    this.openModalForm = function(title, custom_html, cmd) {
        var t = trumbowyg,
        prefix = t.o.prefix,
        lg = t.lang,
        html = '',
        CONFIRM_EVENT = 'tbwconfirm';
        
        html = custom_html;

        return t.openModal(title, html)
            .on(CONFIRM_EVENT, function () {
                var $form = $('form', $(this)),
                valid = true,
                values = $form.serializeArray();
                
                t.restoreRange();
                if (cmd(values)) {
                    t.syncCode();
                    t.$c.trigger('tbwchange');
                    t.closeModal();
                    $(this).off(CONFIRM_EVENT);
                }

            })
            .one('tbwcancel', function () {
                $(this).off(CONFIRM_EVENT);
                t.closeModal();
            });
    };
    */
    
    // DERIVED FROM trumbowyg.js itself
    this.addErrorOnModalField = function ($field, err) {
        var prefix = trumbowyg.o.prefix,
            $label = $field.parent();

        $field
            .on('change keyup', function () {
                $label.removeClass(prefix + 'input-error');
            });

        $label
            .addClass(prefix + 'input-error')
            // SCIPIO: must support select
            //.find('input+span')
            .find('input+span, select+span')
            .append(
                $('<span/>', {
                    class: prefix + 'msg-error',
                    text: err
                })
            );
    };
    
    // DERIVED FROM trumbowyg.js itself (FIXME: duplication with below)
    this.buildInputFieldHtml = function (fieldType, fieldName, fieldValue, fieldLabel, attr) {
        var lg = trumbowyg.lang,
            l = fieldLabel,
            prefix = trumbowyg.o.prefix,
            n = fieldName,
            fieldValue = fieldValue ? fieldValue : '';
        if (!attr) {
            attr = '';
        }
        if (jQuery.type(attr) !== 'string') {
            attr = Object.keys(attr).map(function (prop) {
                return prop + '="' + attr[prop] + '"';
            }).join(' ');
        }
        return '<label><input type="' + fieldType + '" name="' + n + '" value="' + helper.cleanFieldValue(fieldValue) + '"' + attr + '/><span class="' + prefix + 'input-infos"><span>' +
            ((!l) ? (lg[fieldName] ? lg[fieldName] : fieldName) : (lg[l] ? lg[l] : l)) +
            '</span></span></label>';
    };
    
    // DERIVED FROM trumbowyg.js itself (FIXME: duplication with below)
    this.buildSelectFieldHtml = function (fieldName, fieldValue, fieldLabel, attr, valueStrict, selectOptions) {
        var lg = trumbowyg.lang,
            l = fieldLabel,
            prefix = trumbowyg.o.prefix,
            n = fieldName,
            fieldValue = fieldValue ? fieldValue : '';
        if (!attr) {
            attr = '';
        }
        if (jQuery.type(attr) !== 'string') {
            attr = Object.keys(attr).map(function (prop) {
                return prop + '="' + attr[prop] + '"';
            }).join(' ');
        }
        var fieldHtml = '<label><select name="' + n + '" ' + attr + '>';
        if (selectOptions) {
            var optHtml = '';
            var valFound = false;
            // NOTE: tries to keep support for both array and object/map
            jQuery.each(selectOptions, function(ki, opt) {
                optHtml += '<option value="' + helper.cleanFieldValue(opt.value) + '"';
                // FIXME?: test not great, could produce two selecteds
                if (opt.selected === true || (typeof fieldValue !== 'undefined' && fieldValue != null && ((opt.value || '') == (fieldValue || '')))) {
                    optHtml += ' selected="selected"';
                    valFound = true;
                }
                optHtml += '>' + (opt.text || '') + '</option>';
            });
            if (valueStrict !== true && !valFound && (typeof fieldValue !== 'undefined' && fieldValue != null)) {
                fieldHtml += '<option value="' + helper.cleanFieldValue(fieldValue) + '">' + helper.cleanFieldValue(fieldValue) + '</option>';
            }
            fieldHtml += optHtml;
        }
        fieldHtml += '</select><span class="' + prefix + 'input-infos"><span>' +
            ((!l) ? (lg[fieldName] ? lg[fieldName] : fieldName) : (lg[l] ? lg[l] : l)) +
            '</span></span></label>';
        return fieldHtml;
    };
    
    // DERIVED FROM trumbowyg.js itself (FIXME: duplication with below)
    this.cleanFieldValue = function (value) {
        return (value || '').replace(/"/g, '&quot;');
    };
    
    // DERIVED FROM trumbowyg.js itself (FIXME: duplication with below)
    this.makeDummyInputFields = function(namePrefix, count) {
        var res = '',
            prefix = trumbowyg.o.prefix;
        for(var i=1; i <= count; i++) {
            res += '<label><input type="text" name="' + namePrefix + i + '" value=""><span class="' + prefix + 'input-infos"><span>' +
                namePrefix + i + '</span></span></label>';
        }
        return res;
    }
    
    // DERIVED FROM trumbowyg.js itself (FIXME: duplication with above)
    this.openModalInsert = function (title, fields, cmd) {
        var t = trumbowyg,
            prefix = trumbowyg.o.prefix,
            lg = trumbowyg.lang,
            html = '', regHtml = '', advHtml = '',
            CONFIRM_EVENT = 'tbwconfirm';

        $.each(fields, function (fieldName, field) {
            var l = field.label,
                n = field.name || fieldName,
                a = field.attributes || {};

            var attr = Object.keys(a).map(function (prop) {
                return prop + '="' + a[prop] + '"';
            }).join(' ');

            var fieldType = field.type || 'text';
            var cleanValue = function(value) {
                return (value || '').replace(/"/g, '&quot;');
            };
            var fieldHtml = '';
            if (fieldType == 'select') {
                // SCIPIO: new: select support
                fieldHtml += '<label><select name="' + n + '">';
                if (field.selectOptions) {
                    var optHtml = '';
                    var valFound = false;
                    // NOTE: tries to keep support for both array and object/map
                    jQuery.each(field.selectOptions, function(ki, opt) {
                        optHtml += '<option value="' + cleanValue(opt.value) + '"';
                        // FIXME?: test not great, could produce two selecteds
                        if (opt.selected === true || (typeof field.value !== 'undefined' && field.value != null && ((opt.value || '') == (field.value || '')))) {
                            optHtml += ' selected="selected"';
                            valFound = true;
                        }
                        optHtml += '>' + (opt.text || '') + '</option>';
                    });
                    if (field.valueStrict !== true && !valFound && (typeof field.value !== 'undefined' && field.value != null)) {
                        fieldHtml += '<option value="' + cleanValue(field.value) + '">' + cleanValue(field.value) + '</option>';
                    }
                    fieldHtml += optHtml;
                }
                fieldHtml += '</select><span class="' + prefix + 'input-infos"><span>' +
                    ((!l) ? (lg[fieldName] ? lg[fieldName] : fieldName) : (lg[l] ? lg[l] : l)) +
                    '</span></span></label>';
            } else if (fieldType == 'manual') {
                // SCIPIO: hack to support manual
                fieldHtml += field.markup || '';
            } else {
                fieldHtml += '<label><input type="' + fieldType + '" name="' + n + '" value="' + cleanValue(field.value) + '"' + attr + '/><span class="' + prefix + 'input-infos"><span>' +
                    ((!l) ? (lg[fieldName] ? lg[fieldName] : fieldName) : (lg[l] ? lg[l] : l)) +
                    '</span></span></label>';
            }
            if (field.advanced === true) {
                advHtml += fieldHtml;
            } else {
                regHtml += fieldHtml;
            }
        });
        
        if (advHtml) {
            advHtml = '<div class="trumbowyg-adv-fields-section">' 
                + '<h4 class="trumbowyg-scp-field-section-title trumbowyg-adv-fields-title"><a href="javascript:void(0);" class="trumbowyg-scp-field-section-title-link trumbowyg-adv-fields-title-link"><span class="trumbowyg-collapse-switch">[+]</span> ' + lg.scipio_common_advanced + '</a></h4>'
                + '<div class="trumbowyg-adv-fields">'
                + advHtml 
                + "</div>"
                + "</div>";
        }
        html += regHtml + advHtml;

        var modal = t.openModal(title, html)
            .on(CONFIRM_EVENT, function () {
                var $form = $('form', $(this)),
                    valid = true,
                    values = {};

                $.each(fields, function (fieldName, field) {
                    var $field = $('input[name="' + fieldName + '"]', $form);
                    // SCIPIO: also try select
                    var inputType = '';
                    if ($field.length) {
                        inputType = $field.attr('type');
                    } else {
                        $field = $('select[name="' + fieldName + '"]', $form);
                        if ($field.length) {
                            inputType = 'select';
                        }
                    }

                    if (inputType.toLowerCase() === 'checkbox') {
                        values[fieldName] = $field.is(':checked');
                    } else {
                        values[fieldName] = $.trim($field.val());
                    }
                    // Validate value
                    if (field.required && values[fieldName] === '') {
                        valid = false;
                        helper.addErrorOnModalField($field, t.lang.required);
                    } else if (field.pattern && !field.pattern.test(values[fieldName])) {
                        valid = false;
                        helper.addErrorOnModalField($field, field.patternError);
                    }
                });

                if (valid) {
                    t.restoreRange();

                    if (cmd(values, fields)) {
                        t.syncCode();
                        t.$c.trigger('tbwchange');
                        t.closeModal();
                        $(this).off(CONFIRM_EVENT);
                    }
                }
            })
            .one('tbwcancel', function () {
                $(this).off(CONFIRM_EVENT);
                t.closeModal();
            });
        // SCIPIO: hide with jQuery on start and add toggle handler
        jQuery('.trumbowyg-adv-fields', modal).hide();
        jQuery('.trumbowyg-adv-fields-title', modal).click(function() {
            var advFields = jQuery('.trumbowyg-adv-fields', modal);
            var collapseSwitch = jQuery('.trumbowyg-adv-fields-title .trumbowyg-collapse-switch', modal);
            advFields.toggle();
            if (advFields.is(":visible")) {
                collapseSwitch.html('[-]');
            } else {
                collapseSwitch.html('[+]');
            }
        });
        return modal;
    };
    
    /**
     * standard behavior for fetching initial values for form fields.
     */
    this.fromFtlStd = function(k, v, linkType, argDefs, ftlDir, initValues) {
        // NOTE: 2017-03-08: ONLY use the hard initValue if we have no existing directive (ftlDir)
        var initValue = (ftlDir.name || typeof v.initValue === 'undefined') ? null : v.initValue;
        var ftlName = v.ftlName || k;
        var ftlArgsInfo = ftlDir.args[ftlName];
        if (ftlArgsInfo) {
            // NOTE: using the string value, NOT the converted value,
            // because they're all going straight into form field as a string...
            initValue = ftlArgsInfo.strValue;
        }
        initValues[k] = initValue;
    }
    
    /**
     * makes initial values for form fields and adds to initValues map.
     * by default, we try to reuse the parsed ftl defaults from the macro invocation,
     * EVEN if the code is from a different URL macro. 
     * however, we don't allow non-macro URL code.
     * this is flawed, but a lot of the macro arguments are similar so this will allow the user
     * to make corrections rather than starting over.
     */
    this.putFieldInitValues = function(t, linkType, argDefs, ftlDir, initValues) {
        var urlMacroNamesObj = helper.makeUrlMacroNamesObj(true);
        // check if there's possibility of reusing the macro...
        // we'll allow to reuse anything as long as it's one of our known URL macros,
        // even if it's from a different one of ours.
        // this is 
        if (ftlDir && ftlDir.name && ftlDir.type == "macro" && urlMacroNamesObj[ftlDir.name] === true) {
            ; // good
        } else {
            ftlDir = ftlParser.newDirectiveInfo(); // simplifies the rest, compared to: {} or null
        }
        jQuery.each(argDefs, function(k, v) {
            var fromFtl = v.fromFtl || helper.fromFtlStd;
            fromFtl(k, v, linkType, argDefs, ftlDir, initValues);
        });
    };
    
    this.makeFormInputDefs = function(t, initValues, argDefs) {
        var inputDefs = {};
        jQuery.each(argDefs, function(defKey, defVal) {
            inputDefs[defKey] = {
                label : defVal.label,
                value : initValues[defKey],
                required : defVal.required,
                type : defVal.inputType,
                selectOptions : defVal.inputSelectOptions,
                valueStrict : defVal.inputValueStrict,
                onChange : defVal.inputOnChange,
                advanced : defVal.advanced,
                markup : defVal.inputMarkup
            };
        });
        return inputDefs;
    };
    
    this.makeCommonFtlInvokeArgDefs = function(t, v, argDefs) {
        var outFtlDefs = {};
        jQuery.each(argDefs, function(defKey, defVal) {
            outFtlDefs[defVal.ftlName || defKey] = {
                type : defVal.ftlType,
                value : v[defKey],
                toFtl : defVal.toFtl,
                explicit : true
            };
        });
        jQuery.each(v, function(inputName, inputVal) {
            if (typeof outFtlDefs[inputName] === 'undefined' || outFtlDefs[inputName] == null) {
                outFtlDefs[inputName] = {
                    value : inputVal,
                    explicit : false
                };
            }
        });
        return outFtlDefs;
    };
    
    // this is for extra fields added separately from the ones trumbowyg builds and recognizes
    this.makeExtraFtlInvokeArgDefs = function(fields, specialType) {
        var outFtlDefs = {};
        jQuery.each(fields, function(i, field) {
            field = jQuery(field);
            var fieldName = field.attr('name');
            var inputType;
            if (field.prop('tagName').toLowerCase() == 'select') {
                inputType = 'select';
            } else {
                inputType = field.attr('type');
            }
            var ftlType = field.attr('scipio-ftl-type');
            var fieldValue;
            if (inputType.toLowerCase() === 'checkbox') {
                fieldValue = field.is(':checked');
            } else {
                fieldValue = $.trim(field.val());
            }
            outFtlDefs[fieldName] = {
                explicit : false,
                value : fieldValue,
                type : ftlType,
                specialType : specialType
            };
        });
        return outFtlDefs;
    };
    
    this.makeFtlStrLit = function(value) {
        // NOTE: FTL STRINGS MUST BE SINGLE-QUOTES because double-quotes will get 
        // wrecked by the html interpreter invoked through trumbowyg
        return "'" + ftlParser.escapeStringLit(value, "'") + "'";
    };
    
    this.toFtlStd = function(k, v, args) {
        if (v.value) {
            if (v.type == 'boolean') {
                return {name: k, value: v.value}; // no quotes for booleans
            } else {
                return {name: k, value: helper.makeFtlStrLit(v.value)}; // NOTE: MUST BE SINGLE-QUOTES
            }
        } else {
            return null; // skip the arg
        }
    };
    
    // this does NOT automatically make FTL entries for all form inputs, only for the form inputs
    // that were manually defined in arg defs. for the others, you have to define a toFtl
    // that returns one or more values to add.
    this.buildFtlMacroInvoke = function(macroName, args) {
        // NOTE: FTL SYNTAX MUST BE BRACKETS because the < and > will get 
        // wrecked by the html interpreter invoked through trumbowyg
        var res = "";
        var combinedArgs = $.extend({}, args);
        combinedArgs.macroName = macroName;
        jQuery.each(args, function(k, v) {
            if (v.explicit !== false) {
                var toFtl = v.toFtl || helper.toFtlStd;
                var ftlArg = toFtl(k, v, combinedArgs);
                if ($.type(ftlArg) === "array") {
                    $.each(ftlArg, function(i, e) {
                        res += " " + e.name + "=" + e.value;
                    });
                } else if (ftlArg) {
                    res += " " + ftlArg.name + "=" + ftlArg.value;
                }
            }
        });
        res = "[@" + combinedArgs.macroName + res + "/]";
        return res;
    };
    
    this.getWebSiteListAndOptions = function(successCb, allowEmpty) {
        $.ajax({
            url: pluginObj.getWebSitesServerPath,
            type: 'POST',
            data: {},
            cache: false,
            dataType: 'json',
            success: function (data) {
                successCb(data);
            },
            error: pluginObj.error || function () {
                trumbowyg.$c.trigger('tbwscipioerror', [trumbowyg]);
            }
        });  
    };
    
    this.makeWebSiteFieldOptions = function(webSiteList, emptyOpt) {
        var opts = [];
        if (emptyOpt) {
            opts.push(emptyOpt);
        }
        if (webSiteList) {
            jQuery.each(webSiteList, function(i, webSite) {
                opts.push({value:webSite.webSiteId, text: webSite.siteName || webSite.webSiteId});
            });
        }
        return opts;
    };
    
    this.makeContentTypeOptions = function(contentTypeList, emptyOpt) {
        var opts = [];
        if (emptyOpt) {
            opts.push(emptyOpt);
        }
        if (contentTypeList) {
            // FIXME: this bypasses the localization logic of contentType.description
            jQuery.each(contentTypeList, function(i, contentType) {
                opts.push({value:contentType.contentTypeId, text: contentType.description || contentType.contentTypeId});
            });
        }
        return opts;
    };
    
    this.setSelectChangeCallback = function(name, modal, cb, immediate) {
        var select = jQuery('select[name=' + name + ']', modal);
        if (immediate !== false) {
            cb.call(select, modal);
        }
        select.on('change', function() {
            cb.call(this, modal);
        });
        return select;
    };
    
    this.setWebSiteIdChangeCallback = function(modal, cb, immediate) {
        return this.setSelectChangeCallback('webSiteId', modal, cb, immediate);
    };
    
    this.markOptionTextDefault = function(text) {
        return text + ' (' + trumbowyg.lang.scipio_common_default.toLowerCase() + ')';
    };
    
    this.makeOptionArg = function(value, text, defaultVal) {
        if (value === defaultVal) {
            return { value: value, text: helper.markOptionTextDefault(text || value)};
        } else {
            return { value: value, text: text || value};
        }
    };
    
    this.makeOptionArgs = function(args, defaultVal) {
        if (typeof defaultVal === 'undefined') {
            return args;
        } else {
            var res = [];
            jQuery.each(args, function(i, e) {
                var text = e.text || e.value;
                if (e.value === defaultVal) {
                    text = helper.markOptionTextDefault(text);
                }
                res.push({ value: e.value, text: text });
            });
            return res;
        }
    };
    
    this.commonFieldParts = {
        ternaryBoolSelectOptions: function(defaultVal) {
            return [
                helper.makeOptionArg('', '', defaultVal),
                helper.makeOptionArg('true', 'true', defaultVal),
                helper.makeOptionArg('false', 'false', defaultVal)
            ];
        }
    };
    
    this.insertErrorMsgBox = function(errorMsgs, modal) {
        // SCIPIO: special: fallback (but non-fatal) error display
        //errorMsgs.push("Test error message");
        if (errorMsgs && errorMsgs.length > 0) {
            var errorHtml = '<ol>';
            jQuery.each(errorMsgs, function(i, v) {
                errorHtml += '<li>' + v + '</li>';
            });
            errorHtml += '</ol>';
            var errorCntr;
            errorCntr = jQuery('#content-messages-error-template');
            if (errorCntr.length) {
                errorCntr = jQuery('<div class="trumbowyg-scipio-errors trumbowyg-scipio-links-errors"></div>').html(errorCntr.html()); // clone the alert
                jQuery('.content-message-content', errorCntr).html(errorHtml);
            } else {
                errorCntr = jQuery('<div class="trumbowyg-scipio-errors trumbowyg-scipio-links-errors content-message-content"></div>').html(errorHtml);
            }
            jQuery('.trumbowyg-modal-box form', modal).prepend(errorCntr);
        }
    };
    
}
