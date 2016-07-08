package com.ilscipio.scipio.cms.content;

import java.util.Collection;
import java.util.Map;
import java.util.Set;

import javolution.util.FastMap;

/**
 * The content of a page and all its assets with a map interface.
 * 
 * 
 */
public class CmsPageContent implements java.io.Serializable,Map<String, Object>  {


	private static final long serialVersionUID = -6873145124304265390L;
    private CmsPage page;
    private Map<String, Object> map = FastMap.<String, Object>newInstance();

	public static final String module = CmsPageContent.class.getName();


    public CmsPageContent() {
        super();
    }

    public CmsPageContent(Map<String, ?> rootMap, CmsPage page) {        
        map.putAll(rootMap);
        this.page = page;
    }

    public CmsPageContent(CmsPage page) {
        this.page = page;
    }

    
    public CmsPage getPage() {
        return page;
    }
    
    @Override
    public Object get(Object key) {
        Object v = map.get(key);
        if (v != null) { 
            return v;
        } else { 
            return key;
        }
    }

    @SuppressWarnings("unchecked")
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
    
    @SuppressWarnings("unchecked")
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



}
