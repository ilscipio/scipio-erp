package org.ofbiz.commonext.email;

import java.util.HashMap;
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.product.store.ProductStoreWorker;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.ServiceUtil;

/**
 * SCIPIO: Applications extensions of the framework/common component emails, and more.
 */
public abstract class ExtEmailServices {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    protected ExtEmailServices() {
    }

    /**
     * SCIPIO: Send E-Mail From Screen Widget Service (CommonExt version/override).
     * Added 2019-02-01.
     */
    public static Map<String, Object> sendMailFromScreen(DispatchContext dctx, Map<String, Object> context) {
        try {
            return org.ofbiz.common.email.EmailServices.sendMailFromScreen(dctx, normalizeCommonExtEmailFields(dctx, context, true));
        } catch (GenericEntityException e) {
            Debug.logError(e, module);
            return ServiceUtil.returnError(e.toString()); // FIXME
        }
    }

    /**
     * SCIPIO: Attempts to auto-determine and normalize the webSiteId and productStoreId fields in service context
     * and bodyParameters, using each other as well as orderId.
     * <p>
     * This reads, normalizes and sets the following fields, in best-effort fashion:
     * context.webSiteId, bodyParameters.webSiteId, bodyParameters.webSite (optional - may not necessarily set),
     * bodyParameters.productStoreId, bodyParameters.productStore (optional - may not necessarily set),
     * context.orderId, bodyParameters.orderId, bodyParameters.orderHeader (optional - may not necessarily set).
     * <p>
     * IMPORTANT: THIS INTENTIONALLY TREATS MISSING KEY AS DIFFERENT FROM NULL VALUE IN MAPS, especially for ID fields.
     * Entity values are treated less strictly and are only updated if we happened to look them up, in order
     * to allow the templates to reuse the lookups if they want.
     */
    public static Map<String, Object> normalizeCommonExtEmailFields(DispatchContext dctx, Map<String, Object> context, boolean contextReadOnly) throws GenericEntityException {
        if (Boolean.FALSE.equals(context.get("autoInferParams"))) {
            return context;
        }
        return new FieldHelper(dctx, context, contextReadOnly).processContext();
    }

    /**
     * SCIPIO: Field value helper - use a single instance and after each call, fetch back valueSet flg and the value.
     * Helps avoid double entity lookups and unnecessary hash lookups.

     */
    protected static final class FieldHelper {
        private static final String paramsContextKey = "bodyParameters";
        private DispatchContext dctx;
        private boolean contextReadOnly;
        private Map<String, Object> context;
        private Map<String, Object> params;
        private boolean hadParams;
        private Map<String, Object> writableContext;
        private Map<String, Object> writableParams;

        private FieldInfo<String> webSiteId;
        private FieldInfo<String> productStoreId;
        private FieldInfo<String> orderId;

        public FieldHelper(DispatchContext dctx, Map<String, Object> context, boolean contextReadOnly) {
            this.dctx = dctx;
            this.contextReadOnly = contextReadOnly;
            this.context = context;
            Map<String, Object> params = UtilGenerics.cast(context.get(paramsContextKey));
            this.hadParams = (params != null);
            this.params = (params != null) ? params : new HashMap<>(); // NOTE: No need to copy bodyParameters; sendMailFromScreen edits it in-place
        }

        public Map<String, Object> processContext() throws GenericEntityException { // TODO?: Could re-implement this method's logic using FieldInfo subclasses, but this is fine for now
            webSiteId = new WebSiteIdField();
            productStoreId = new ProductStoreIdField();
            orderId = new OrderIdField();

            // Get productStoreId and webSiteId from order or from each other
            if (!productStoreId.isSet() || !webSiteId.isSet()) {
                GenericValue orderHeader = orderId.getOrQueryEntityValue(false);
                webSiteId.setValueFromEntityField(orderHeader, "webSiteId");
                productStoreId.setValueFromEntityField(orderHeader, "productStoreId");

                // Get productStoreId from website
                if (!productStoreId.isSet()) {
                    productStoreId.setValueFromEntityField(webSiteId.getOrQueryEntityValue(true), "productStoreId");
                }

                // If no webSiteId, get DEFAULT webSiteId from product store
                if (!webSiteId.isSet() && productStoreId.getValue() != null) {
                    webSiteId.setValueNonNull(ProductStoreWorker.getStoreWebSiteIdForEmail(dctx.getDelegator(),
                            productStoreId.getValue(), webSiteId.getValue(), true));
                }
            }

            webSiteId.store();
            productStoreId.store();
            orderId.store();

            return getProcessedContext();
        }

        public Map<String, Object> getContext() { return context; }
        public Map<String, Object> getParams() { return params; }
        public <T> T getParam(String name) { return UtilGenerics.cast(getParams().get(name)); }
        public Map<String, Object> getWritableContext() {
            if (writableContext == null) {
                writableContext = contextReadOnly ? new HashMap<>(context) : context;
            }
            return writableContext;
        }
        public Map<String, Object> getWritableParams() {
            if (writableParams == null) {
                writableParams = params;
                if (!hadParams) {
                    getWritableContext().put(paramsContextKey, params);
                }
            }
            return writableParams;
        }
        public Map<String, Object> getProcessedContext() { return (writableContext != null) ? writableContext : context; }

        public abstract class FieldInfo<T> {
            protected final FieldMeta meta;
            protected boolean valueSet;
            protected T value; // The value (webSiteId, productStoreId, ...)
            protected boolean contextSet; // true if was set in context
            protected boolean paramSet; // true if was set in bodyParameters
            protected GenericValue entityValue; // cached entity
            protected Boolean entitySet; // true if entity was set in bodyParameters, false if not, or null if not checked yet

            protected FieldInfo(FieldMeta meta) { this.meta = meta; }

            protected final void initAll() { // webSiteId, orderId
                initFromContext();
                initFromParams();
                initEntityFromParams();
            }

            protected final void initAllNoContext() { // productStoreId
                initFromParams();
                initEntityFromParams();
            }

            protected final void initFromContext() { // NOTE: this assumes it's the first one checked, if it's used.
                value = UtilGenerics.cast(getContext().get(meta.contextKey));
                contextSet = UtilMisc.containsKey(getContext(), meta.contextKey, value); // NOTE: Optimized version of containsKey
                valueSet = contextSet;
            }

            protected final void initFromParams() {
                Object paramValue = getParam(meta.paramKey);
                paramSet = UtilMisc.containsKey(getParams(), meta.paramKey, paramValue);
                if (!valueSet && paramSet) {
                    valueSet = true;
                    value = UtilGenerics.cast(paramValue);
                }
            }

            protected final void initEntityFromParams() {
                if (!valueSet) { // Delay entity check unless have no value
                    GenericValue entityValue = getEntityValue();
                    if (entityValue != null) {
                        value = UtilGenerics.cast(entityValue.get(meta.entityFieldKey));
                        valueSet = (value != null);
                    }
                }
            }

            protected final void initEntityDisabled() {
                entitySet = false; // non-null makes getEntityValue return immediately
            }

            /* TODO?: maybe later if logic complexity increases...
            protected final void resolveDeps() throws GenericEntityException {
                if (!isSet()) {
                    resolveDepsCore();
                }
            }
            protected abstract void resolveDepsCore() throws GenericEntityException;
            */

            public final boolean isSet() { return valueSet; }
            public final T getValue() { return value; }
            public final boolean isContextSet() { return contextSet; }
            public final boolean isParamSet() { return paramSet; }
            public final Boolean getEntityParamSet() { return entitySet; }

            protected final GenericValue getEntityValue() { // Gets existing value from bodyParameters
                if (entitySet != null) { // NOTE: non-null means the lookup was already made.
                    return entityValue;
                }
                entityValue = getParam(meta.entityParamKey);
                entitySet = UtilMisc.containsKey(getParams(), meta.entityParamKey, entityValue);
                return entityValue;
            }

            protected final boolean isEntitySet() { // NOTE: Does not save result.
                return (entitySet != null) ? entitySet : getParams().containsKey(meta.entityParamKey);
            }

            public final GenericValue getOrQueryEntityValue(boolean useCache) throws GenericEntityException {
                if (getEntityValue() != null || getValue() == null) {
                    return entityValue;
                }
                entityValue = dctx.getDelegator().findOne(meta.entityName, UtilMisc.toMap(meta.entityFieldKey, getValue()), useCache);
                //entitySet = entityValue != null; // Don't replace this, only meant to flag if bodyParameters contained entity originally
                return entityValue;
            }

            public final void setValue(T value) {
                this.value = value;
                this.valueSet = true;
            }
            public final void setValueNonNull(T value) { if (value != null) { setValue(value); } }

            public final void setValueFromEntityField(GenericValue entityValue, String fieldName) {
                if (entityValue != null) {
                    Object value = entityValue.get(fieldName);
                    if (value != null) {
                        setValue(UtilGenerics.cast(value));
                    }
                }
            }

            public void store() { storeAll(); } // Default implementation: store to everything

            public final void storeAll() { // webSiteId, orderId
                if (isSet()) {
                    if (!isContextSet()) {
                        getWritableContext().put(meta.contextKey, value);
                    }
                    if (!isParamSet()) {
                        getWritableParams().put(meta.paramKey, value);
                    }
                }
                if (entityValue != null) {
                    getWritableParams().put(meta.entityParamKey, entityValue);
                }
            }

            public final void storeAllNoContext() { // productStoreId
                if (isSet()) {
                    if (!isParamSet()) {
                        getWritableParams().put(meta.paramKey, value);
                    }
                }
                if (entityValue != null) {
                    getWritableParams().put(meta.entityParamKey, entityValue);
                }
            }
        }

        public final class WebSiteIdField extends FieldInfo<String> {
            WebSiteIdField() {
                super(FieldMeta.WEB_SITE_ID);
                initAll();
            }
        }

        public final class ProductStoreIdField extends FieldInfo<String> { // The productStoreId must not get stored to service context - bodyParameters only.
            ProductStoreIdField() {
                super(FieldMeta.PRODUCT_STORE_ID);
                initAllNoContext();
            }
            @Override
            public void store() { storeAllNoContext(); }
        }

        public final class OrderIdField extends FieldInfo<String> {
            OrderIdField() {
                super(FieldMeta.ORDER_ID);
                initAll();
            }
        }

        private static final class FieldMeta { // TODO?: remove this in favor of proper FieldInfo subclass methods
            public static final FieldMeta WEB_SITE_ID = new FieldMeta("webSiteId", "webSiteId", "webSite", "webSiteId", "WebSite");
            public static final FieldMeta PRODUCT_STORE_ID = new FieldMeta(null, "productStoreId", "productStore", "productStoreId", "ProductStore");
            public static final FieldMeta ORDER_ID = new FieldMeta("orderId", "orderId", "orderHeader", "orderId", "OrderHeader");

            private final String contextKey;
            private final String paramKey;
            private final String entityParamKey;
            private final String entityFieldKey;
            private final String entityName;

            FieldMeta(String contextKey, String paramKey, String entityParamKey, String entityFieldKey, String entityName) {
                this.contextKey = contextKey;
                this.paramKey = paramKey;
                this.entityParamKey = entityParamKey;
                this.entityFieldKey = entityFieldKey;
                this.entityName = entityName;
            }
        }
    }
}
