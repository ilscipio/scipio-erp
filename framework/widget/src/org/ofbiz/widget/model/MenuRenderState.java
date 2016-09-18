package org.ofbiz.widget.model;

import java.util.Map;

/**
 * SCIPIO: a state passed around in context used to record info about the menu
 * render, such as sub-menu depth.
 */
public class MenuRenderState {
    
    public static final String CONTEXT_KEY = "menuStringRender_renderState";
    
    protected int currentDepth;
    protected int maxDepth;
    
    protected MenuRenderState() {
        this.currentDepth = 1;
        this.maxDepth = -1;
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
    
    public void increaseCurrentDepth() {
        this.currentDepth++;
    }
    
    public void decreaseCurrentDepth() {
        this.currentDepth--;
    }
    
    public boolean hasReachedMaxDepth() {
        return (maxDepth >= 0) && (currentDepth >= maxDepth);
    }
    
    public static MenuRenderState create() {
        return new MenuRenderState();
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