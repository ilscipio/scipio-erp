package com.ilscipio.scipio.product.store;

import com.ilscipio.scipio.base.util.AttrHandler;
import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.product.store.ProductStoreWorker;

import javax.servlet.ServletContext;
import java.util.Locale;
import java.util.Map;
import java.util.TimeZone;

public class StoreAttrHandler extends AttrHandler {

    public StoreAttrHandler(ServletContext servletContext) {
        super(servletContext);
    }

    public static class Factory extends AttrHandler.Factory {
        @Override
        public StoreAttrHandler make(ServletContext servletContext) {
            return new StoreAttrHandler(servletContext);
        }
    }

    @Override
    protected StoreResolver resolver() {
        return new StoreResolver();
    }

    public class StoreResolver extends Resolver {
        protected GenericValue productStore;
        protected boolean cacheProductStore = true;

        @Override
        public StoreAttrHandler handler() {
            return StoreAttrHandler.this;
        }

        // Options

        /**
         * Sets explicit store to use, instead of current session store.
         */
        public StoreResolver productStore(GenericValue productStore) {
            this.productStore = productStore;
            return this;
        }

        public StoreResolver cacheProductStore(boolean cacheProductStore) {
            this.cacheProductStore = cacheProductStore;
            return this;
        }

        @Override
        public StoreResolver userLogin(Map<String, Object> userLogin) {
            return (StoreResolver) super.userLogin(userLogin);
        }

        @Override
        public StoreResolver checkUserLogin(boolean checkUserLogin) {
            return (StoreResolver) super.checkUserLogin(checkUserLogin);
        }

        @Override
        public StoreResolver checkClientRequest(boolean checkClientRequest) {
            return (StoreResolver) super.checkClientRequest(checkClientRequest);
        }

        // Operations

        @Override
        public Locale resolveDefaultLocale() {
            GenericValue productStore = getProductStore();
            if (productStore != null && productStore.get("defaultLocaleString") != null) {
                return UtilMisc.asLocale(productStore.get("defaultLocaleString"));
            }
            return super.resolveDefaultLocale();
        }

        @Override
        public Locale filterLocale(Object locale, Boolean exact) {
            GenericValue productStore = getProductStore();
            if (productStore != null) {
                return ProductStoreWorker.getStoreCandidateLocale(productStore, locale, exact);
            }
            return super.filterLocale(locale, exact);
        }

        @Override
        public TimeZone resolveDefaultTimeZone() {
            GenericValue productStore = getProductStore();
            if (productStore != null && productStore.get("defaultTimeZoneString") != null) {
                return UtilDateTime.asTimeZone(productStore.get("defaultTimeZoneString"));
            }
            return super.resolveDefaultTimeZone();
        }

        @Override
        public TimeZone filterTimeZone(Object timeZone, Boolean exact) {
            // Probably no real need to filter time zones
            return super.filterTimeZone(timeZone, exact);
        }

        @Override
        public String resolveDefaultCurrencyUom() {
            GenericValue productStore = getProductStore();
            if (productStore != null && productStore.get("defaultCurrencyUomId") != null) {
                return (String) productStore.get("defaultCurrencyUomId");
            }
            return super.resolveDefaultCurrencyUom();
        }

        @Override
        public String filterCurrencyUom(Object currencyUom, Boolean exact) {
            GenericValue productStore = getProductStore();
            if (productStore != null) {
                return ProductStoreWorker.getStoreCandidateCurrencyUom(productStore, currencyUom, exact);
            }
            return super.filterCurrencyUom(currencyUom, exact);
        }

        @Override
        public void setDefaultSessionSettings() {
            super.setDefaultSessionSettings(); // Handles locale and timeZone

            UtilHttp.setCurrencyUom(session, resolveCurrencyUom());
        }

        @Override
        public void setDefaultUserSettings() {
            super.setDefaultUserSettings(); // Handles locale and timeZone

            String currencyUom = resolveExplicitCurrencyUom();
            if (currencyUom != null) {
                UtilHttp.setCurrencyUom(session, currencyUom);
            } else {
                currencyUom = resolveImplicitCurrencyUom();
                if (currencyUom != null) {
                    UtilHttp.setCurrencyUomIfNone(session, currencyUom);
                } else {
                    currencyUom = resolveDefaultCurrencyUom();
                    if (currencyUom != null) {
                        UtilHttp.setCurrencyUomIfNone(session, currencyUom);
                    }
                }
            }
        }

        protected GenericValue getProductStore() {
            GenericValue productStore = this.productStore;
            if (productStore == null) {
                if (request != null) {
                    productStore = ProductStoreWorker.getProductStore(request);
                } else if (session != null) {
                    productStore = ProductStoreWorker.getProductStore(session);
                }
                if (cacheProductStore) {
                    this.productStore = productStore;
                }
            }
            return productStore;
        }
    }

    public static void productStore(AttrHandler.Resolver attrResolver, GenericValue productStore) {
        if (attrResolver instanceof StoreAttrHandler.StoreResolver) {
            ((StoreAttrHandler.StoreResolver) attrResolver).productStore(productStore);
        }
    }

}
