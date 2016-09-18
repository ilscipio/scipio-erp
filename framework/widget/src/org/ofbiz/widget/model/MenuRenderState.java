package org.ofbiz.widget.model;

import java.io.Serializable;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

/**
 * SCIPIO: a state passed around in context used to record info about the menu
 * render, such as sub-menu depth.
 */
@SuppressWarnings("serial")
public class MenuRenderState implements Map<String, Object>, Serializable {
    
    public static final String CONTEXT_KEY = "currentMenuRenderState";
    
    private final Map<String, Object> internalMap = new HashMap<String, Object>();
    // some operations will be read-only, to prevent issues
    private final Map<String, Object> readOnlyMap = Collections.unmodifiableMap(internalMap);

    // NOTE: these are also stored in the map, may change the redundancy later
    private int currentDepth;
    private int maxDepth;
    private String subMenuFilter;
    private transient boolean noSubMenus;
    private transient boolean currentSubMenusOnly;
    private transient ModelMenu.MenuAndItem selectedMenuAndItem;
    
    protected MenuRenderState() {
        setCurrentDepth(1);
        setMaxDepth(1);
        setSubMenuFilter(null);
        selectedMenuAndItem = null;
    }
    
    protected Object setArg(String key, Object value) {
        if ("currentDepth".equals(key)) {
            this.setCurrentDepth((Integer) value);
            return this.get(key);
        } else if ("maxDepth".equals(key)) {
            this.setMaxDepth((Integer) value);
            return this.get(key);
        } else if ("subMenuFilter".equals(key)) {
            this.setSubMenuFilter((String) value);
            return this.get(key);
        } else {
            return setInternal(key, value);
        }
    }
    
    protected void setArgs(Map<? extends String, ?> args) {
        for(Map.Entry<? extends String, ?> entry : args.entrySet()) {
            this.setArg(entry.getKey(), entry.getValue());
        }
    }
    
    protected Object setInternal(String key, Object value) {
        return internalMap.put(key, value);
    }
    
    public static MenuRenderState create() {
        return new MenuRenderState();
    }
    
    public int getCurrentDepth() {
        return currentDepth;
    }
    
    protected void setCurrentDepth(Integer currentDepth) {
        if (currentDepth == null) {
            this.currentDepth = 1;
        } else {
            this.currentDepth = currentDepth;
        }
        setInternal("currentDepth", this.currentDepth);
    }
    
    public int getMaxDepth() {
        return maxDepth;
    }
    
    public void setMaxDepth(Integer maxDepth) {
        if (maxDepth == null) {
            this.maxDepth = -1;
        } else {
            this.maxDepth = maxDepth;
        }
        setInternal("maxDepth", this.maxDepth);
    }
    
    public String getSubMenuFilter() {
        return subMenuFilter;
    }

    public void setSubMenuFilter(String subMenuFilter) {
        this.subMenuFilter = subMenuFilter;
        this.noSubMenus = "none".equals(subMenuFilter);
        this.currentSubMenusOnly = "current".equals(subMenuFilter);
        setInternal("subMenuFilter", this.subMenuFilter);
    }

    public void increaseCurrentDepth() {
        this.currentDepth++;
        setInternal("currentDepth", this.currentDepth);
    }
    
    public void decreaseCurrentDepth() {
        this.currentDepth--;
        setInternal("currentDepth", this.currentDepth);
    }
    
    public boolean hasReachedMaxDepth() {
        return noSubMenus || ((maxDepth >= 0) && (currentDepth >= maxDepth));
    }
    
    public boolean isCurrentSubMenusOnly() {
        return currentSubMenusOnly;
    }
    
    public boolean isInlineEntries() {
        return Boolean.TRUE.equals(this.get("inlineEntries"));
    }
    
    public String getMenuCtxRole() {
        return (String) this.get("menuCtxRole");
    }
    
    public String getMenuCtxRoleOrEmpty() {
        String res = (String) this.get("menuCtxRole");
        return res != null ? res : "";
    }

    /**
     * Gets selected submenu/item pair from model menu and caches for this render.
     */
    public ModelMenu.MenuAndItem getSelectedMenuAndItem(ModelMenu modelMenu, Map<String, Object> context) {
        if (this.selectedMenuAndItem == null) {
            this.selectedMenuAndItem = modelMenu.getSelected(context);
        }
        return this.selectedMenuAndItem;
    }
    
    // context helper methods
    
    public static MenuRenderState createAndStore(Map<String, Object> context) {
        MenuRenderState renderState = create();
        store(context, renderState);
        return renderState;
    }
    
    public static void store(Map<String, Object> context, MenuRenderState renderState) {
        context.put(CONTEXT_KEY, renderState);
    }
    
    public static boolean hasRenderState(Map<String, Object> context) {
        return (retrieve(context) != null);
    }
    
    public static MenuRenderState retrieve(Map<String, Object> context) {
        return (MenuRenderState) context.get(CONTEXT_KEY);
    }
    
    public static MenuRenderState remove(Map<String, Object> context) {
        return (MenuRenderState) context.remove(CONTEXT_KEY);
    }

    @Override
    public int size() {
        return internalMap.size();
    }

    @Override
    public boolean isEmpty() {
        return internalMap.isEmpty();
    }

    @Override
    public boolean containsKey(Object key) {
        return internalMap.containsKey(key);
    }

    @Override
    public boolean containsValue(Object value) {
        return internalMap.containsValue(value);
    }

    @Override
    public Object get(Object key) {
        return internalMap.get(key);
    }

    @Override
    public Object put(String key, Object value) {
        return setArg(key, value);
    }

    @Override
    public Object remove(Object key) {
        throw new UnsupportedOperationException();
    }

    @Override
    public void putAll(Map<? extends String, ? extends Object> m) {
        setArgs(m);
    }

    @Override
    public void clear() {
        throw new UnsupportedOperationException();
    }

    @Override
    public Set<String> keySet() {
        return readOnlyMap.keySet();
    }

    @Override
    public Collection<Object> values() {
        return readOnlyMap.values();
    }

    @Override
    public Set<java.util.Map.Entry<String, Object>> entrySet() {
        return readOnlyMap.entrySet();
    }


}