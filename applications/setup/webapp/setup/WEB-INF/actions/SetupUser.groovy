import org.ofbiz.base.util.Debug
import org.ofbiz.product.store.ProductStoreWorker


if (context.productStoreId) {
    productStore = ProductStoreWorker.getProductStore(context.productStoreId, delegator);

    context.createAllowPassword = "Y".equals(productStore.allowPassword);
    context.getUsername = !"Y".equals(productStore.usePrimaryEmailUsername);
}