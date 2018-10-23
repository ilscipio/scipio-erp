/**
 * SCIPIO: Category simple text content localized fields lookup.
 */

import org.ofbiz.base.util.*;
import org.ofbiz.entity.util.*;
import org.ofbiz.service.ServiceUtil;
import org.ofbiz.content.content.LocalizedContentWorker;
 
final module = "GetCategoryStcLocFields.groovy";
 
catalogLocFieldsInfo = context.catalogLocFieldsInfo;
if (!catalogLocFieldsInfo) {
    GroovyUtil.runScriptAtLocation("component://product/webapp/catalog/WEB-INF/actions/catalog/GetCatalogLocFieldsInfo.groovy", null, context);
    catalogLocFieldsInfo = context.catalogLocFieldsInfo;
    context.catalogLocFieldsInfo = catalogLocFieldsInfo;
}

// form field initial value logic - read from params if update error
catStclfReadFromParams = context.catStclfReadFromParams; // override
if (catStclfReadFromParams == null) catStclfReadFromParams = (context.isError && "Y".equals(parameters.updatePccStcLocFields));
context.catStclfReadFromParams = catStclfReadFromParams;

catStclfParams = context.catStclfParams;
if (catStclfParams == null) catStclfParams = parameters;
context.catStclfParams = catStclfParams;

productCategoryId = context.productCategoryId;
categoryStcViewsByType = [:];
try {
    if (catStclfReadFromParams) {
        categoryStcViewsByType = LocalizedContentWorker.parseLocalizedSimpleTextContentFieldParams(catStclfParams, "contentFields_", false);
    } else {
        if (productCategoryId) {
            def servRes = dispatcher.runSync("getProductCategoryContentLocalizedSimpleTextViews", [
                productCategoryId: productCategoryId, prodCatContentTypeIdList: catalogLocFieldsInfo.category.typeNames,
                userLogin:context.userLogin, locale:context.locale, timeZone:context.timeZone
            ], -1, true);
            if (!ServiceUtil.isSuccess(servRes)) {
                context.stcErrorMsg = ServiceUtil.getErrorMessage(servRes);
            } else {
                categoryStcViewsByType = servRes.viewsByType;
            }
        }
    }
} catch(Exception e) {
    // DEV NOTE: DO NOT set context.isError/errorMessageList from here - keep separate
    PropertyMessage msgIntro = PropertyMessage.makeWithVars("ProductErrorUiLabels", 
                "productservices.error_reading_ProductCategoryContent_simple_texts_for_alternate_locale_for_category",
                "productCategoryId", productCategoryId);
    Debug.logError(e, PropertyMessageExUtil.makeLogMessage(msgIntro, e), module);
    context.stcErrorMsg = PropertyMessageExUtil.makeServiceMessage(msgIntro, e, context.locale);
}
context.categoryStcViewsByType = categoryStcViewsByType;
