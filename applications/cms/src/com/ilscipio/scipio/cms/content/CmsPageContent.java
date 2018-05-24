package com.ilscipio.scipio.cms.content;

import java.io.Serializable;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.TimeZone;

import org.ofbiz.base.util.UtilProperties;

import com.ilscipio.scipio.cms.data.Preloadable;
import com.ilscipio.scipio.cms.data.Preloadable.AbstractPreloadable;
import com.ilscipio.scipio.cms.template.AttributeExpander;
import com.ilscipio.scipio.cms.template.AttributeExpander.TypeParser;
import com.ilscipio.scipio.cms.template.CmsAttributeTemplate;
import com.ilscipio.scipio.cms.template.CmsAttributeTemplate.Type;

/**
 * The content of a page and all its assets with a map interface.
 * <p>
 * NOTE: 2016: during rendering an immutable copy is kept on the page for caching, and
 * duplicates are made for each render. 
 * WARN: however the duplicate is SHALLOW and must
 * be done at every new asset, and only guarantees consistency on the first level (FIXME?)...
 * (doing deep-copies is unreliable and slower... TODO?: investigate if you could MapStack here...)
 */
public class CmsPageContent extends AbstractPreloadable implements Serializable, Preloadable, Map<String, Object> {

    private static final long serialVersionUID = -6873145124304265390L;
    
    //private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    
    private static final String useNamesForMissingAttributesStr = UtilProperties.getPropertyValue("cms.properties", "render.attributes.useNamesForMissing", "never");
    private static final boolean useNamesForMissingAttributesAlways = "always".equals(useNamesForMissingAttributesStr);
    private static final boolean useNamesForMissingAttributesPreview = "preview".equals(useNamesForMissingAttributesStr);

    private final CmsPage page;
    private Map<String, Object> map = new HashMap<>();

    public CmsPageContent(Map<String, ?> rootMap, CmsPage page) {        
        map.putAll(rootMap);
        this.page = page;
    }

    public CmsPageContent(CmsPage page) {
        this.page = page;
    }
    
    /**
     * shallow copy constructor (first level only).
     */
    public CmsPageContent(CmsPageContent other) {
        this.page = other.page;
        this.map.putAll(other.map);
    }

    @Override
    public void preload(PreloadWorker preloadWorker) {
        super.preload(preloadWorker);
        
        // NOTE: don't handle this.getPage() - not our job
        this.map = preloadWorker.transformContainer(this.map);
    }
    
    /**
     * Create in-memory shallow copy of page content (first level only).
     */
    public CmsPageContent clone() {
        return new CmsPageContent(this);
    }
    
    public boolean isUseNamesForMissingAttributes(CmsPageContext pageContext) {
        return useNamesForMissingAttributesAlways || (useNamesForMissingAttributesPreview && pageContext.isPreview());
    }
    
    public CmsPage getPage() {
        return page;
    }
    
    /**
     * Gets asset content by creating a shallow duplicate (important!).
     */
    public CmsPageContent getAssetContent(String assetName) {
        return getAssetContent(assetName, true);
    }
    
    @SuppressWarnings("unchecked")
    public CmsPageContent getAssetContent(String assetName, boolean inherit) {
        Object assetContent = get(assetName);
        CmsPageContent content = new CmsPageContent(this.page);
        if (inherit) {
            content.putAll(this);
        }
        if (assetContent instanceof Map) {
            content.putAll((Map<String, ?>) assetContent);
        }
        return content;
    }
    
    public CmsPageContent setAssetContent(String assetName, CmsPageContent assetContent) {
        put(assetName, assetContent);
        CmsPageContent content = new CmsPageContent(this, this.page);
        return content;
    }
    
    public Map<String, Map<String, ?>> getProducts() {
        return page.getProducts();
    }

    public Map<String, ?> getProduct(String name) {
        return page.getProduct(name);
    }

    public Object getRaw(Object key) {
        return map.get(key);
    }
    
    
    /* ********************************************************** */
    /* java.util.Map interface methods */
    /* ********************************************************** */
    
    @Override
    public Object get(Object key) {
        Object v = map.get(key);
        // NOTE: 2017: this no longer works safely from this code...
        // instead we will rely on injectVariableContent method 
        // to re-insert the keys as values only IF the type is string,
        // because this will break handling of non-string variables in templates.
        // this class is not aware of the attribute types.
//        if (isUseNamesForMissingAttributes()) {
//            if (v != null) { 
//                return v;
//            } else { 
//                return key;
//            }
//        } else {
//            return v;
//        }
        return v;
    }
    
    @Override
    public void clear() {
        map.clear();    
    }

    @Override
    public boolean containsKey(Object key) {        
        return map.containsKey(key);
    }

    @Override
    public boolean containsValue(Object value) {        
        return map.containsValue(value);
    }

    @Override
    public Set<java.util.Map.Entry<String, Object>> entrySet() {
        return map.entrySet();
    }

    @Override
    public boolean isEmpty() {
        return map.isEmpty();
    }

    @Override
    public Set<String> keySet() {
        return map.keySet();
    }

    @Override
    public Object put(String key, Object value) {
        return map.put(key, value);
    }

    @Override
    public void putAll(Map<? extends String, ? extends Object> vm) {
        map.putAll(vm);        
    }

    @Override
    public Object remove(Object key) {
        return map.remove(key);
    }

    @Override
    public int size() {
        return map.size();
    }

    @Override
    public Collection<Object> values() {
        return map.values();
    }
    
    
    /* ********************************************************** */
    /* Rendering setup methods */
    /* ********************************************************** */

    /**
     * Normalizes this content to at very least contain keys for all the passed attributes (for a given page/template/asset).
     * It will also add default values for any null or missing keys, for attributes that have default values.
     * (default value must be non-null, non-empty and non-blank to be considered).
     * <p>
     * NOTE: 2017: it is EXTREMELY important to set null values in context so all attributes are accounted
     * for as keys; this guarantees all attributes will be reset in context when they are transferred
     * to context. if didn't do this, because the context is reused across asset calls (new 2017 - REQUIRED),
     * then attributes would risk receiving random values from unrelated renders.
     * NOTE: this is largely an issue because of the top-level context dump; if referred to content
     * only using a namespace (like the existing cmsContent.xxx map, which can be used also),
     * there would not be this issue.
     * <p>
     * This does NOT perform any variable substitutions; see {@link #parseExpandAttributes} for that.
     * <p>
     * NOTE: currently edits the passed CmsPageContent in-place. currently (2017-02) there is no reason to create a new one...
     * 
     * @deprecated WE NOW PROCESS the list of attributes in the rendering and there is no more putAll operation
     */
    @Deprecated
    public CmsPageContent normalizeForAttributes(List<CmsAttributeTemplate> attributeTemplates, CmsPageContext pageContext) {
        CmsPageContent content = this;
        for (CmsAttributeTemplate at : attributeTemplates) {
            String name = at.getName();
            Object value = content.getRaw(name);
            if (value != null) {
                // TODO: conversion
            } else {
                // TODO
                // just putting null here for now, to register the key...
                content.put(name, null);
//                // Store the default value, if any, converted, but WITHOUT any expansions.
//                // NOTE: even if there is no default, this put is essential
//                // to register the key.
//                Object convertedDefaultValue = AttributeExpander.getNolangExpander().parse(at.getType(), 
//                        at.getCleanedDefaultValue(), Collections.<String, Object> emptyMap(), pageContext);
//                content.put(name, convertedDefaultValue);
            }
        }
        return content;
    }

    /**
     * Parses and expands the given attribute, storing it back inside this page content,
     * as well as returning the value.
     */
    public Object parseExpandAttribute(CmsAttributeTemplate at, Map<String, ?> sourceContext, CmsPageContext pageContext, boolean resultToSelf) {
        CmsPageContent content = this;
        
        final boolean useNamesForMissing = content.isUseNamesForMissingAttributes(pageContext);
        // DEV NOTE: 2017-02: we MUST ALWAYS override the values for all attribute names defined even if null,
        // with either null or the variable name (for basic types only), so that the keys are present.
        // this is because the context is (and MUST be) now inherited, while was not in the past.
        // we only have this issue because there is no namespace for the attributes (2017-02-08).
        Object newVal = null;
        String defVal = at.getCleanedDefaultValue();
        Type type = at.getType();
       
        try {
            // Get expander and parser
            AttributeExpander expander = at.getExpander();
            TypeParser typeParser = expander.getParserAlways(type);
            
            // Get value
            Object val = content.getRaw(at.getName());
            if (val == null && defVal != null) {
                // if there is no value set, we use the default values
                val = defVal;
            }
            
            // Parse
            newVal = typeParser.validateParse(val, at, sourceContext, pageContext);
            
            // 2017-04-11: support type conversion
            String targetType = at.getTargetType();
            if (newVal != null && targetType != null && !targetType.isEmpty()) {
                newVal = AttributeExpander.convertToJavaType(newVal, targetType, (TimeZone) sourceContext.get("timeZone"), (Locale) sourceContext.get("locale"));
            }

            if (useNamesForMissing && type.isStringType() && val == null) {
                // If enabled in config, can print out attribute names for missing values 
                // (only makes sense with strings).
                // NOTE: 2017: this replaces the old CmsPageContent.get handling of returning key names
                // Also, it is DISABLED by default.
                newVal = at.getName();
            }
        } catch(Throwable t) {
            throw new IllegalArgumentException("Cms Attribute: Error parsing attribute with name '" + at.getName() 
                + "' of type '" + type + "' (id: " + at.getId() + "): " + t.getMessage(), t);
        }
        
        if (resultToSelf) {
            content.put(at.getName(), newVal);
        }
        return newVal;
    }
    
    /**
     * Parses and expands the content attributes, using the language defined on each CmsAttributeTemplate.
     * <p>
     * NOTE: currently edits the passed CmsPageContent in-place. currently (2017-02) there is no reason to create a new one...
     * 
     * @deprecated the renderer now parses CmsAttributeTemplate one-by-one, using {@link #parseExpandAttribute}; this was too simplistic
     */
    @Deprecated
    public CmsPageContent parseExpandAttributes(List<CmsAttributeTemplate> attributeTemplates, Map<String, ?> sourceContext, CmsPageContext pageContext) {
        for (CmsAttributeTemplate at : attributeTemplates) {
            parseExpandAttribute(at, sourceContext, pageContext, true);
        }
        return this;
    }

    /**
     * Transfer this page content to rendering screen context, ONLY for keys which
     * exist in this page content, but NOT necessarily all attributes defined by the page/template/asset.
     * 
     * @deprecated probably avoid using this; the renderer handles CmsAttributeTemplate on individual basis now
     */
    @Deprecated
    public void transferToContext(Map<String, Object> context, Set<String> namesToSkip) {
        for (Map.Entry<String, Object> entry : entrySet()) {
            String key = entry.getKey();
            if (!namesToSkip.contains(key)) {
                context.put(key, entry.getValue());
            }
        }
    }
    
}
