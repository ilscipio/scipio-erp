import org.ofbiz.base.util.GroovyUtil
import org.ofbiz.base.util.UtilValidate
import org.ofbiz.entity.util.EntityQuery
import org.ofbiz.product.store.ProductStoreWorker

/**
 * SCIPIO NOTE: Currently this only supports maileon, but it could be extended to support other newsletter services.
 */

isMaileonComponentPresent = org.ofbiz.base.component.ComponentConfig.isComponentPresent("maileon")
if (UtilValidate.isEmpty(isMaileonComponentPresent)) {
    isMaileonComponentPresent = false
}

productStoreId = ProductStoreWorker.getProductStoreId(request)
productStoreMaileon = null
if (isMaileonComponentPresent && productStoreId) {
    productStoreMaileon = EntityQuery.use(delegator).from("ProductStoreMaileon").where("productStoreId", productStoreId).queryOne()
    GroovyUtil.runScriptAtLocation("component://maileon/script/MaileonCustomFields.groovy", null, context)
}
context.productStoreMaileon = productStoreMaileon
context.isMaileonComponentPresent = isMaileonComponentPresent
context.maileonRenderEmail = true