package com.ilscipio.scipio.solr;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.apache.solr.common.SolrDocument;
import org.ofbiz.entity.GenericValue;

/**
 * SCIPIO: Helper that wraps around a single solr query result value and provides
 * Scipio-schema specific operations.
 * NOT thread-safe.
 */
public class SolrValueWorker {

    private final SolrDocument solrDoc;
    private final Locale currentLocale;
    private final Locale fallbackLocale;
    private final List<Locale> lookupLocales;
    
    /* 
     ******************************************
     * Constructors
     ******************************************
     */
    
    protected SolrValueWorker(SolrDocument solrDoc, Locale currentLocale, Locale fallbackLocale) {
        this.solrDoc = solrDoc;
        this.currentLocale = currentLocale;
        this.fallbackLocale = fallbackLocale;
        this.lookupLocales = makeNonNullList(this.currentLocale, this.fallbackLocale); // this is not really appropriate for stores: , SolrUtil.getSolrContentLocaleDefault(null, null)
    }

    public static SolrValueWorker getWorker(SolrDocument solrDoc, Locale currentLocale, Locale fallbackLocale) {
        return new SolrValueWorker(solrDoc, SolrLocaleUtil.getCompatibleLocale(currentLocale), SolrLocaleUtil.getCompatibleLocale(fallbackLocale)); 
    }
    
    public static SolrValueWorker getWorker(SolrDocument solrDoc, Locale currentLocale, GenericValue productStore) {
        return new SolrValueWorker(solrDoc, SolrLocaleUtil.getCompatibleLocale(currentLocale), SolrLocaleUtil.getCompatibleProductStoreLocaleValid(productStore)); 
    }
    
    public static SolrValueWorker getWorker(SolrDocument solrDoc) {
        return new SolrValueWorker(solrDoc, null, null); 
    }
    
    public SolrDocument getSolrDoc() {
        return solrDoc;
    }

    /* 
     ******************************************
     * Delegate methods
     ******************************************
     */
    
    public Object getFieldValue(String name) {
        return solrDoc.getFieldValue(name);
    }
    
    public Collection<String> getFieldNames() {
        return solrDoc.getFieldNames();
    }

    public Map<String, Object> getFieldValueMap() {
        return solrDoc.getFieldValueMap();
    }

    public Collection<Object> getFieldValues(String arg0) {
        return solrDoc.getFieldValues(arg0);
    }

    public Map<String, Collection<Object>> getFieldValuesMap() {
        return solrDoc.getFieldValuesMap();
    }

    public Object getFirstValue(String name) {
        return solrDoc.getFirstValue(name);
    }

    
    /* 
     ******************************************
     * Custom methods
     ******************************************
     */
    
    /**
     * Abstracted helper method to return the default field value or if not set its <code>*_i18n_*</code> version.
     * THIS EMULATES THE {@link org.ofbiz.product.product.ProductContentWrapper} BEHAVIOR!
     * This behavior is currently the same as: {@link #getFieldValueDefaultOrI18n}
     * So the fields specified directly on Product entity take priority over the localized versions!
     * Yes, this is weird! It means the general value should be omitted if you want the localized content to work.
     */
    public Object getFieldValueI18nForDisplay(String name) {
        return getFieldValueGeneralOrI18n(name);
    }
    
    /**
     * Abstracted method to return appropriate-locale value for one of the <code>*_i18n_*</code>
     * or equivalent fields for the given locale.
     * This version NEVER returns a language other than the ones passed to this worker's constructor.
     * name is the first part of the field name without underscores, 
     * e.g. "title", "description", "longdescription".
     */
    public Object getFieldValueI18nStrict(String name) {
        for(Locale locale : lookupLocales) {
            Object value = solrDoc.getFieldValue(name + "_i18n_" + SolrLocaleUtil.getLangCode(locale));
            if (value != null) return value;
        }
        return null;
    }
    
    /**
     * Abstracted method to return the default non-localized version of the given <code>*_i18n_*</code> field.
     */
    public Object getFieldValueGeneral(String name) {
        return solrDoc.getFieldValue(name + "_i18n_" + SolrLocaleUtil.I18N_GENERAL);
    }
    
    /**
     * Abstracted method to return appropriate-locale value for one of the <code>*_i18n_*</code>
     * or equivalent fields for the given locale.
     * If the languages passed to this constructor are unavailable or don't have values, this will
     * return a default text in another language (if possible) as a default/fallback.
     * name is the first part of the field name without underscores, 
     * e.g. "title", "description", "longdescription".
     */
    public Object getFieldValueI18nOrGeneral(String name) {
        Object value = getFieldValueI18nStrict(name);
        if (value != null) return value;
        return getFieldValueGeneral(name);
    }
    
    /**
     * Abstracted method to return the general field value or if not set its <code>*_i18n_*</code> version.
     * NOTE: at time of writing, this 
     * name is the first part of the field name without underscores, 
     * e.g. "title", "description", "longdescription".
     */
    public Object getFieldValueGeneralOrI18n(String name) {
        Object value = getFieldValueGeneral(name);
        if (value != null) return value;
        return getFieldValueI18nStrict(name);
    }


    /* 
     ******************************************
     * Internal helpers
     ******************************************
     */
    
    @SafeVarargs
    static <T> List<T> makeNonNullList(T... values) {
        List<T> res = new ArrayList<>();
        for(T value : values) {
            if (value != null) res.add(value);
        }
        return res;
    }
    
}
