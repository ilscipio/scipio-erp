/**
 * SCIPIO: Describes the simple text content ProductContent and ProductCategoryContent field types.
 * TODO: REVIEW: I'm not sure the best place to store this yet.
 */
import org.ofbiz.base.util.UtilProperties;
 
categoryFields = [
  [typeName:"CATEGORY_NAME", fieldName:"categoryName", "inputType":"SHORT_TEXT"],
  [typeName:"DESCRIPTION", fieldName:"description", "inputType":"SHORT_TEXT"],
  [typeName:"LONG_DESCRIPTION", fieldName:"longDescription", "inputType":"LONG_TEXT"]
];

// NOTE: default label is ("FormFieldTitle_"+fieldName), see ftl #getCatalogLocFieldLabel
productFields = [
  [typeName:"PRODUCT_NAME", fieldName:"productName", "inputType":"SHORT_TEXT", "label":"ProductProductName"],
  [typeName:"DESCRIPTION", fieldName:"description", "inputType":"SHORT_TEXT"],
  [typeName:"LONG_DESCRIPTION", fieldName:"longDescription", "inputType":"LONG_TEXT"]
];

// this is used for passing list param to service event handler which convert back to list
def makeListString(list) {
    return "[" + list.join(", ") + "]";
}

def compileFieldsInfo(fields, manual) {
    def res = new HashMap();
    Iterable<String> typeNames = fields.collect{ it.typeName };
    Iterable<String> fieldNames = fields.collect{ it.fieldName };
    res.put("typeNames", typeNames);
    res.put("fieldNames", fieldNames);
    res.put("typeNameListStr", "[" + typeNames.join(", ") + "]");
    res.put("fieldNameListStr", "[" + fieldNames.join(", ") + "]");
    def typeInfoMap = new HashMap();
    fields.each{ typeInfoMap[it.typeName] = it; }
    res.put("typeInfoMap", typeInfoMap);
    if (manual) res.putAll(manual);
    return res;
}

// DEV NOTE: this structure gets printed out to JS (public), so don't put internals in here
catalogLocFieldsInfo = [
    general: [
        localeOpts: [expandCountries:UtilProperties.getPropertyAsBoolean("catalog", 
                            "catalogLocFieldsInfo.general.localeOpts.expandCountries", true)]
    ],
    category: compileFieldsInfo(categoryFields, [typeField:"prodCatContentTypeId"]),
    product: compileFieldsInfo(productFields, [typeField:"productContentTypeId"])
];
context.catalogLocFieldsInfo = catalogLocFieldsInfo;

//org.ofbiz.base.util.Debug.logInfo("catalogLocFieldsInfo: " + catalogLocFieldsInfo, "GetCatalogLocFieldsInfo.groovy");

