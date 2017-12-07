/**
 * SCIPIO: Common content JS, for helpers that can apply to any applications, such
 * as localizable fields.
 * DEV NOTE: do not add any actions on file run (e.g. document ready), classes and functions only.
 */

/**
 * Generic content form/field util.
 */
function ScpCntFormUtil() {
}
ScpCntFormUtil.extractClassNameSuffix = function(elem, prefix) {
    var classes = elem.attr('class').split(/\s+/);
    var result = null;
    var startsWith = function(str, prefix) {
        return (str.lastIndexOf(prefix, 0) === 0);
    };
    jQuery.each(classes, function(i, e) {
        if (startsWith(e, prefix)) {
            result = e.substring(prefix.length);
            return false;
        }
    });
    return result;
};

/**
 * A map of jQuery selectors that returns jQuery-wrapped cloned markup.
 */
function ScpCntFormMarkup(selectors) {
    this.selectors = selectors;
    
    this.getSelMarkup = function(name, defaultMarkupStr) {
        return ScpCntFormMarkup.getCntMarkup(this.selectors[name] ? jQuery(this.selectors[name]) : null, defaultMarkupStr);
    };
    this.getCntMarkup = function(markupCnt, defaultMarkupStr) {
        return ScpCntFormMarkup.getCntMarkup(markupCnt, defaultMarkupStr);
    };
}
ScpCntFormMarkup.getCntMarkup = function(markupCnt, defaultMarkupStr) {
    if (markupCnt && markupCnt.length) {
        return markupCnt.children().clone(true, true);
    }
    return defaultMarkupStr ? jQuery(defaultMarkupStr) : null;
};

/**
 * Simple text content localized field handler (stcLocField).
 * NOTE: The basic stcLocField only needs the handleFieldAdd function,
 * the rest is for JS-heavy tree integration.
 */
function StcLocFieldHandler(data) {
    var slfh = this;
 
    var reportInternalError = function(msg) {
        alert("Internal error (ui): " + msg);
    };
    
    this.makeLocFieldNamePrefix = function(paramNamePrefix, typeName, index) {
        return paramNamePrefix + typeName + '.' + index + '.';
    };
    
    this.getLocFieldDataCntElem = function(fieldCnt) {
        if (fieldCnt.hasClass('stc-locfield-data')) return fieldCnt;
        var elem = jQuery('.stc-locfield-data', fieldCnt);
        if (elem.length) return elem;
        else return fieldCnt; // else assume it's the container itself
    };
    
    this.getLocFieldDataValue = function(fieldCnt, attrName) {
        return slfh.getLocFieldDataCntElem(fieldCnt).data(attrName);
    };
    
    this.getLocFieldDataValueRequired = function(fieldCnt, attrName) {
        var value = slfh.getLocFieldDataValue(fieldCnt, attrName);
        if (!value) {
            reportInternalError('missing data-'+attrName+' html custom attribute on localized field');
            return null;
        }
        return value;
    };
    
    this.findFieldCntForTypeName = function(typeName, allFieldsCnt) {
        var dataCnt = jQuery('*[data-stclf-type-name="'+typeName+'"]', allFieldsCnt);
        if (!dataCnt.length) {
            return null; // form doesn't support
        }
        var fieldCnt;
        if (dataCnt.hasClass('stc-locfield')) {
            fieldCnt = dataCnt;
        } else {
            fieldCnt = dataCnt.closest('.stc-locfield');
            if (!fieldCnt.length) {
                reportInternalError('missing stc-locfield class on localized field');
                return null;
            }
        }
        return fieldCnt;
    };
    
    /**
     * needs at least either: fieldCnt OR (typeName AND allFieldsCnt)
     */
    this.getLocalizedFieldProps = function(fieldCnt, typeName, allFieldsCnt) {
        if (!fieldCnt) {
            if (!typeName || !allFieldsCnt) {
                reportInternalError('invalid getLocalizedFieldProps call');
                return null;
            }
            fieldCnt = slfh.findFieldCntForTypeName(typeName, allFieldsCnt);
            if (!fieldCnt) return null;
        } else if (!typeName) {
            typeName = slfh.getLocFieldDataValueRequired(fieldCnt, 'stclfTypeName');
            if (!typeName) return null;
        }

        var paramNamePrefix = slfh.getLocFieldDataValueRequired(fieldCnt, 'stclfParamNamePrefix');
        
        // NOTE: template markup is embedded in the html, now under the field itself (due to styling workaround)
        var entryMarkupTmpl = jQuery('.stc-markup-locFieldEntry:first', fieldCnt);
        if (!entryMarkupTmpl.length) {
            reportInternalError('missing stc-markup-locFieldEntry html-embedded template');
            return null;
        }

        var entries = jQuery('.stc-locfield-entries', fieldCnt);
        if (!entries.length) {
            reportInternalError('missing stc-locfield-entries container');
            return null;
        }
        return { fieldCnt:fieldCnt, typeName:typeName, entryMarkupTmpl:entryMarkupTmpl, 
            entries:entries, paramNamePrefix:paramNamePrefix };
    };
    
    this.buildLocalizedFieldEntry = function(fieldProps, index, entryData) {
        var entryMarkupTmpl = fieldProps.entryMarkupTmpl,
            paramNamePrefix = fieldProps.paramNamePrefix,
            typeName = fieldProps.typeName;
        
        var entryMarkup = ScpCntFormMarkup.getCntMarkup(entryMarkupTmpl);
        if (!entryMarkup || !entryMarkup.length) return null;
        var namePrefix = slfh.makeLocFieldNamePrefix(paramNamePrefix, typeName, index);
        jQuery('.stc-locfield-locale', entryMarkup).attr('name', namePrefix+'localeString').val(entryData.localeString || '');
        jQuery('.stc-locfield-text', entryMarkup).attr('name', namePrefix+'textData').val(entryData.textData || '');
        return entryMarkup;
    };

    this.removeLocalizedFieldEntries = function(fieldProps) {
        fieldProps.entries.empty();
    };
    
    this.rebuildLocalizedFieldEntries = function(fieldProps, entryDataList) {
        fieldProps.entries.empty();
        
        if (entryDataList && entryDataList.length) {
            // add the main/default entry (Product[Category]Content, index zero) + ContentAssoc entries
            jQuery.each(entryDataList, function(index, entryData) {
                var entryMarkup = slfh.buildLocalizedFieldEntry(fieldProps, index, entryData);
                if (entryMarkup) {
                    fieldProps.entries.append(entryMarkup);
                }
            });
        } else {
            // add empty main/default entry (Product[Category]Content)
            var entryMarkup = slfh.buildLocalizedFieldEntry(fieldProps, 0, {});
            if (entryMarkup) {
                fieldProps.entries.append(entryMarkup);
            }
        }
    };
    
    this.removeAllLocalizedFieldEntries = function(allFieldsCnt, typeNames, entryDataListsByType) {
        jQuery.each(typeNames, function(index, typeName) {
            var fieldProps = slfh.getLocalizedFieldProps(null, typeName, allFieldsCnt);
            if (fieldProps) { // if null, either error or not supported by form
                slfh.removeLocalizedFieldEntries(fieldProps, entryDataListsByType[typeName]);
            }
        });
    };
    
    this.rebuildAllLocalizedFieldEntries = function(allFieldsCnt, typeNames, entryDataListsByType) {
        jQuery.each(typeNames, function(index, typeName) {
            var fieldProps = slfh.getLocalizedFieldProps(null, typeName, allFieldsCnt);
            if (fieldProps) { // if null, either error or not supported by form
                slfh.rebuildLocalizedFieldEntries(fieldProps, entryDataListsByType[typeName]);
            }
        });
    };

    this.addLocalizedFieldEntry = function(fieldProps, entryData) {
        if (!entryData) entryData = {}; // adds empty
        
        var index = jQuery('.stc-locfield-entry', fieldProps.fieldCnt).length; // starts at zero
        
        var entryMarkup = slfh.buildLocalizedFieldEntry(fieldProps, index, entryData);
        if (entryMarkup) {
            fieldProps.entries.append(entryMarkup);
        }
    };
    
    this.handleFieldAdd = function(linkElem) {
        linkElem = jQuery(linkElem);
        var fieldCnt = linkElem.closest('.stc-locfield');
        if (fieldCnt.length) {
            var fieldProps = slfh.getLocalizedFieldProps(fieldCnt);
            if (!fieldProps) return;
            slfh.addLocalizedFieldEntry(fieldProps, {});
        } else {
            reportInternalError('missing stc-locfield class on localized field');
        }
    };
    
    /**
     * DEV NOTE: This INTENTIONALLY does not remove duplicates, because it can happen through
     * system or user error. This allows user to see the error, and when submit it should correct itself;
     * this is better than texts silently disappearing.
     */
    this.parseViewsByType = function(viewsByType) {
        /* don't have to do anything; names already match
        var entryDataListsByType = {};
        jQuery.each(viewsByType, function(typeName, viewList) {
            var entryDataList = [];
            if (viewList) {
                for(var i=0; i < viewList.length; i++) {
                    var view = viewList[i];
                    entryDataList.push({
                        localeString:view.localeString,
                        textData:view.textData
                    });
                }
            }
            entryDataListsByType[typeName] = entryDataList;
        });
        return entryDataListsByType;
        */
        return viewsByType;
    };
}
// Default instance (used to have properties, not required anymore)
var stcLocFieldHandler = new StcLocFieldHandler({});

    