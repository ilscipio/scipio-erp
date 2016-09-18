package org.ofbiz.widget.model;

import java.io.Serializable;
import java.util.Map;

/**
 * SCIPIO: a state passed around in context used to record info about the menu
 * render, such as sub-menu depth.
 */
public class MenuRenderState {
    
    public static final String CONTEXT_KEY = "menuStringRender_renderState";
    
    private int currentDepth;
    private int maxDepth;
    private String subMenuFilter;
    private transient boolean noSubMenus;
    private transient boolean activeSubMenusOnly;
    
    protected MenuRenderState() {
        this.currentDepth = 1;
        this.maxDepth = -1;
        this.subMenuFilter = null;
        this.noSubMenus = false;
        this.activeSubMenusOnly = false;
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
    }
    
    public String getSubMenuFilter() {
        return subMenuFilter;
    }

    public void setSubMenuFilter(String subMenuFilter) {
        this.subMenuFilter = subMenuFilter;
        this.noSubMenus = "none".equals(subMenuFilter);
        this.activeSubMenusOnly = "active".equals(subMenuFilter);
    }

    public void increaseCurrentDepth() {
        this.currentDepth++;
    }
    
    public void decreaseCurrentDepth() {
        this.currentDepth--;
    }
    
    public boolean hasReachedMaxDepth() {
        return noSubMenus || ((maxDepth >= 0) && (currentDepth >= maxDepth));
    }
    
    public boolean isActiveSubMenusOnly() {
        return activeSubMenusOnly;
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
}