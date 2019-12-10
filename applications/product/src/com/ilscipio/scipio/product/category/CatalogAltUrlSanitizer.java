package com.ilscipio.scipio.product.category;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.entity.GenericValue;

public abstract class CatalogAltUrlSanitizer {

    /**
     * Clean and convert a name to alt url for storage as ALTERNATIVE_URL.
     */
    public abstract String convertNameToDbAltUrl(String name, Locale locale, CatalogUrlType entityType, SanitizeContext ctxInfo);

    /**
     * Clean and convert an id to alt url for storage as ALTERNATIVE_URL - may be called as fallback when
     * no name found (depending on config), or may be left to live.
     */
    public abstract String convertIdToDbAltUrl(String id, Locale locale, CatalogUrlType entityType, SanitizeContext ctxInfo);

    /**
     * Clean and convert a name to alt url for storage as ALTERNATIVE_URL - returns new map with keys converted.
     * NOTE: last, nameIndex and totalNames may be null.
     */
    public Map<String, String> convertNamesToDbAltUrls(Map<String, String> map, CatalogUrlType entityType, SanitizeContext ctxInfo) {
        Map<String, String> newMap = new HashMap<>();
        for(Map.Entry<String, String> entry : map.entrySet()) {
            newMap.put(entry.getKey(), convertNameToDbAltUrl(entry.getValue(), parseLocale(entry.getKey()), entityType, ctxInfo));
        }
        return newMap;
    }

    /**
     * Cleans alt url coming out of DB IF not already applied before going into db.
     * This is a TRADEOFF with storage-time cleaning.
     * <p>
     * WARN: 2017-11-08: FOR SEO URLs, THIS MUST DO NOTHING AND RETURN pathPart AS-IS!
     * This will allow better DB queries. See implementation.
     * <p>
     * WARN: the locale here might not perfectly match the pathPart due to performance reasons!!
     * More reliable in other methods. See FIXME in SeoCatalogUrlWorker.
     * NOTE: last, nameIndex and totalNames may be null.
     */
    public abstract String sanitizeAltUrlFromDb(String altUrl, Locale locale, CatalogUrlType entityType, SanitizeContext ctxInfo);

    /**
     * Converts an ID (productId, etc.) directly to a live alt URL part.
     * WARN: In most cases this method should be left to do nothing! The DB IDs should
     * only contain simple characters to begin with.
     * NOTE: last, nameIndex and totalNames may be null.
     */
    public abstract String convertIdToLiveAltUrl(String id, Locale locale, CatalogUrlType entityType, SanitizeContext ctxInfo);

    /**
     * TODO: REVIEW: this doesn't belong here but it's the only way to make the parsing consistent.
     */
    public Locale parseLocale(String localeString) {
        return UtilMisc.parseLocale(localeString);
    }


    public static abstract class Factory<T extends CatalogAltUrlSanitizer> implements Serializable {
        public abstract T getInstance();
        public abstract T getInstance(Map<String, Object> options);
    }

    public SanitizeContext makeSanitizeContext() {
        return new SanitizeContext();
    }

    /**
     * Class to pass arguments to methods above, needed to prevent compatibility breakage.
     * NOTE: Unless otherwise specified, all fields may be null.
     */
    public static class SanitizeContext implements Serializable {
        private Boolean last;
        private Integer nameIndex;
        private Integer totalNames;
        private GenericValue targetProduct;
        private GenericValue targetCategory;

        protected SanitizeContext(Boolean last, Integer nameIndex, Integer totalNames) { // NOTE: avoid using this
            this.last = last;
            this.nameIndex = nameIndex;
            this.totalNames = totalNames;
        }

        public SanitizeContext() {
        }

        public static SanitizeContext undefined() {
            return ReadOnlySanitizeContext.UNDEFINED;
        }

        public static SanitizeContext lastElem(boolean last) {
            return last ? ReadOnlySanitizeContext.LAST : ReadOnlySanitizeContext.NON_LAST;
        }

        public static SanitizeContext lastElem() {
            return ReadOnlySanitizeContext.LAST;
        }

        public static SanitizeContext nonLastElem() {
            return ReadOnlySanitizeContext.NON_LAST;
        }

        public Boolean getLast() {
            return last;
        }

        public boolean isLast() {
            return Boolean.TRUE.equals(last);
        }

        public boolean isNonLast() {
            return Boolean.FALSE.equals(last);
        }

        public SanitizeContext setLast(Boolean last) {
            this.last = last; return this;
        }

        public Integer getNameIndex() {
            return nameIndex;
        }

        public SanitizeContext setNameIndex(Integer nameIndex) {
            this.nameIndex = nameIndex; return this;
        }

        public int increaseNameIndex() {
            this.nameIndex++;
            return nameIndex;
        }

        public Integer getTotalNames() {
            return totalNames;
        }

        public SanitizeContext setTotalNames(Integer totalNames) {
            this.totalNames = totalNames; return this;
        }

        public String getTargetProductId() {
            return (getTargetProduct() != null) ? getTargetProduct().getString("productId") : null;
        }

        public GenericValue getTargetProduct() { return targetProduct; }

        public SanitizeContext setTargetProduct(GenericValue targetProduct) {
            this.targetProduct = targetProduct; return this;
        }

        public String getTargetCategoryId() { return (getTargetCategory() != null) ? getTargetCategory().getString("productCategoryId") : null; }

        public GenericValue getTargetCategory() { return targetCategory; }

        public SanitizeContext setTargetCategory(GenericValue targetCategory) {
            this.targetCategory = targetCategory; return this;
        }

        public static class ReadOnlySanitizeContext extends SanitizeContext {
            private static final SanitizeContext UNDEFINED = new ReadOnlySanitizeContext(null, null, null);
            private static final SanitizeContext LAST = new ReadOnlySanitizeContext(true, null, null);
            private static final SanitizeContext NON_LAST = new ReadOnlySanitizeContext(false, null, null);

            public ReadOnlySanitizeContext(Boolean last, Integer nameIndex, Integer totalNames) {
                super(last, nameIndex, totalNames);
            }

            @Override
            public SanitizeContext setLast(Boolean last) {
                throw new UnsupportedOperationException();
            }

            @Override
            public SanitizeContext setNameIndex(Integer nameIndex) {
                throw new UnsupportedOperationException();
            }

            @Override
            public SanitizeContext setTotalNames(Integer totalNames) {
                throw new UnsupportedOperationException();
            }

            @Override
            public SanitizeContext setTargetProduct(GenericValue targetProduct) { throw new UnsupportedOperationException(); }

            @Override
            public SanitizeContext setTargetCategory(GenericValue targetCategory) { throw new UnsupportedOperationException(); }
        }
    }
}
