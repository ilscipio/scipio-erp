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
        alert("Internal error: simple text content localized fields: " + msg);
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
    
    this.getLocFieldDataValueAlways = function(fieldCnt, attrName) {
        var value = slfh.getLocFieldDataCntElem(fieldCnt).data(attrName);
        if (!value) throw new Error('missing data-'+attrName+' html custom attribute on localized field');
        return value;
    };
    
    this.findFieldCntForTypeName = function(typeName, allFieldsCnt) {
        var dataCnt = jQuery('*[data-stclf-type-name="'+typeName+'"]', allFieldsCnt);
        if (!dataCnt.length) {
            return null; // form doesn't support - not an error
        }
        var fieldCnt;
        if (dataCnt.hasClass('stc-locfield')) {
            fieldCnt = dataCnt;
        } else {
            fieldCnt = dataCnt.closest('.stc-locfield');
            if (!fieldCnt.length) {
                throw new Error('missing stc-locfield class on localized field');
            }
        }
        return fieldCnt;
    };
    
    /**
     * Constructor.
     */
    var LocalizedFieldProps = function(fieldCnt, typeName, allFieldsCnt) {
        if (!fieldCnt) {
            if (!typeName || !allFieldsCnt) {
                throw new Error('invalid getLocalizedFieldProps call arguments');
            }
            fieldCnt = slfh.findFieldCntForTypeName(typeName, allFieldsCnt);
            if (!fieldCnt) {
                // if returned null, means form doesn't support the type - this is plausible (not error)
                this.typeUnsupported = true
                return;
            }
        } else if (!typeName) {
            typeName = slfh.getLocFieldDataValueAlways(fieldCnt, 'stclfTypeName');
        }
        this.fieldCnt = fieldCnt;
        this.typeName = typeName;

        this.paramNamePrefix = slfh.getLocFieldDataValueAlways(fieldCnt, 'stclfParamNamePrefix');
        
        this.markup = {};
        // NOTE: template markup is embedded in the html, now under the field itself (due to styling workaround)
        this.markup.mainEntry = jQuery('.stc-locfield-markup-mainEntry:first', fieldCnt);
        if (!this.markup.mainEntry.length) {
            throw new Error('missing stc-locfield-markup-mainEntry html-embedded template');
        }
        
        var altEntry = jQuery('.stc-locfield-markup-altEntry:first', fieldCnt);
        if (!altEntry.length) {
            altEntry = this.markup.mainEntry;
        }
        this.markup.altEntry = altEntry;

        this.entries = jQuery('.stc-locfield-entries', fieldCnt);
        if (!this.entries.length) {
            throw new Error('missing stc-locfield-entries container');
        }
    };  
    
    /**
     * needs at least either: fieldCnt OR (typeName AND allFieldsCnt)
     */
    this.getLocalizedFieldProps = function(fieldCnt, typeName, allFieldsCnt) {
        try {
            var fieldProps = new LocalizedFieldProps(fieldCnt, typeName, allFieldsCnt);
            if (fieldProps.typeUnsupported) return null;
            return fieldProps;
        } catch(e) {
            reportInternalError("error initializing localized fields: " + e.message);
            return null;
        } 
    };
    
    var optionsContainValue = function(options, value) {
        var found = false;
        jQuery.each(options, function(i, e) {
            if(jQuery(e).val() === value) {
                found = true;
                return false;
            }
        });
        return found;
    };
    
    this.buildLocalizedFieldEntry = function(fieldProps, entryIndex, entryData, strictLocale) {
        var entryMarkup = ScpCntFormMarkup.getCntMarkup((entryIndex == 0) ? fieldProps.markup.mainEntry : fieldProps.markup.altEntry);
        if (!entryMarkup || !entryMarkup.length) return null;
        var namePrefix = slfh.makeLocFieldNamePrefix(fieldProps.paramNamePrefix, fieldProps.typeName, entryIndex);
        
        var localeString = entryData.localeString || '';
        var localeSelectElem = jQuery('.stc-locfield-locale', entryMarkup);
        if (strictLocale !== true) {
            // SPECIAL: Must check if locale already in list; if not we must add a new emergency
            // entry, otherwise we misrepresent the system state and confuse the user.
            // THIS INCLUDES BLANK - if DB contained a blank locale at a non-first entry, we must show that to user so
            // he can fix it.
            if (!optionsContainValue(jQuery('option', localeSelectElem), localeString)) {
                // FIXME: we don't have the locale label in this case... but not a serious issue
                localeSelectElem.prepend('<option value="'+localeString+'" selected="selected">'+localeString+'</option>');
            }
        }
        localeSelectElem.attr('name', namePrefix+'localeString').val(localeString);
        jQuery('.stc-locfield-text', entryMarkup).attr('name', namePrefix+'textData').val(entryData.textData || '');
        return entryMarkup;
    };

    this.removeLocalizedFieldEntries = function(fieldProps) {
        fieldProps.entries.empty();
    };
    
    this.rebuildLocalizedFieldEntries = function(fieldProps, entryDataList) {
        fieldProps.entries.empty();
        
        if (entryDataList && entryDataList.length) {
            // add the main/default entry (Product[Category]Content, index zero) + alt entries (ContentAssoc ALTERNATE_LOCALE)
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
        if (!entryDataListsByType) entryDataListsByType = {};
        jQuery.each(typeNames, function(index, typeName) {
            var fieldProps = slfh.getLocalizedFieldProps(null, typeName, allFieldsCnt);
            if (fieldProps) { // if null, either error or not supported by form
                slfh.removeLocalizedFieldEntries(fieldProps, entryDataListsByType[typeName]);
            }
        });
    };
    
    this.rebuildAllLocalizedFieldEntries = function(allFieldsCnt, typeNames, entryDataListsByType) {
        if (!entryDataListsByType) entryDataListsByType = {};
        jQuery.each(typeNames, function(index, typeName) {
            var fieldProps = slfh.getLocalizedFieldProps(null, typeName, allFieldsCnt);
            if (fieldProps) { // if null, either error or not supported by form
                slfh.rebuildLocalizedFieldEntries(fieldProps, entryDataListsByType[typeName]);
            }
        });
    };

    this.addLocalizedFieldEntry = function(fieldProps, entryData, strictLocale) {
        if (!entryData) entryData = {}; // adds empty
        
        var index = jQuery('.stc-locfield-entry', fieldProps.fieldCnt).length; // starts at zero
        
        var entryMarkup = slfh.buildLocalizedFieldEntry(fieldProps, index, entryData, strictLocale);
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
            slfh.addLocalizedFieldEntry(fieldProps, null, true);
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
        // currently nothing to do here; field names (localeString, textData) already match
        return viewsByType;
    };
}
// Default instance (used to have properties, not required anymore)
var stcLocFieldHandler = new StcLocFieldHandler({});

    