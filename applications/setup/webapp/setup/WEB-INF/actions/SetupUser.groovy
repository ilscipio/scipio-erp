import org.ofbiz.base.util.Debug
import org.ofbiz.product.store.ProductStoreWorker

//productStore = ProductStoreWorker.getProductStore(request);
//context.productStoreId = productStore.productStoreId;
//context.productStore = productStore;

Debug.log("product store id =========> " + context.productStoreId);
productStore = ProductStoreWorker.getProductStore(context.productStoreId, delegator);

context.createAllowPassword = "Y".equals(productStore.allowPassword);
context.getUsername = !"Y".equals(productStore.usePrimaryEmailUsername);