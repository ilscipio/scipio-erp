package org.ofbiz.widget.model;

import java.io.Serializable;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.collections.CompositeReadOnlyMap;
import org.ofbiz.widget.model.ModelMenu.MenuAndItem;

/**
 * SCIPIO: a state passed around in context used to record info about the menu
 * render, such as sub-menu depth.
 * <p>
 * NOTE: this probably didn't need to be serializable, but is just in case
 */
@SuppressWarnings("serial")
public class MenuRenderState extends CompositeReadOnlyMap<String, Object> implements Serializable {
    
    public static final String CONTEXT_KEY = "currentMenuRenderState";
    
    private final ModelMenu modelMenu;
    // NOTE: these are also stored in the map, may change the redundancy later
    private int currentDepth;
    private int maxDepth;
    private String subMenuFilter;
    private transient boolean noSubMenus;
    private transient boolean currentSubMenusOnly;
    private transient ModelMenu.MenuAndItem selectedMenuAndItem;
    private transient ModelMenu.FlaggedMenuNodes flaggedMenuNodes; // TODO? not being used
    
    private transient MenuItemState itemState;

    protected MenuRenderState(Map<String, Object> context, ModelMenu modelMenu) {
        this.modelMenu = modelMenu;
        setCurrentDepth(1);
        setMaxDepth(1);
        setSubMenuFilter(null);
        this.selectedMenuAndItem = null;
        this.flaggedMenuNodes = null;
        this.itemState = null;
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
    
    public static MenuRenderState create(Map<String, Object> context, ModelMenu modelMenu) {
        return new MenuRenderState(context, modelMenu);
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
    
    public MenuItemState getItemState() {
        return itemState;
    }
    
    public void setItemState(MenuItemState menuItemState) {
        this.itemState = menuItemState;
        this.put("itemState", menuItemState);
    }

    /**
     * Gets selected submenu/item pair from model menu from cache.
     */
    public ModelMenu.MenuAndItem getSelectedMenuAndItem(Map<String, Object> context) {
        if (this.selectedMenuAndItem == null) {
            this.selectedMenuAndItem = modelMenu.getSelectedMenuAndItem(context);
        }
        return this.selectedMenuAndItem;
    }
    
    public void updateSelectedMenuAndItem(Map<String, Object> context) {
        this.selectedMenuAndItem = modelMenu.getSelectedMenuAndItem(context);
        
        // 2016-11-11: also determine all the possible manually-selected items, which might get crazy...
        this.flaggedMenuNodes = ModelMenu.FlaggedMenuNodes.resolve(context, modelMenu.getManualSelectedNodes(), 
                modelMenu.getManualExpandedNodes(), this.selectedMenuAndItem);
    }
    
    // context helper methods
    
    public static MenuRenderState createAndStore(Map<String, Object> context, ModelMenu modelMenu) {
        MenuRenderState renderState = create(context, modelMenu);
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
    public Object put(String key, Object value) {
        return setArg(key, value);
    }
    
    @Override
    public void putAll(Map<? extends String, ? extends Object> m) {
        setArgs(m);
    }

    public static class MenuItemState extends CompositeReadOnlyMap<String, Object> implements Serializable {
        private boolean selected;
        private boolean selectedAncestor;
        private Boolean conditionResult;
        
        public MenuItemState(boolean selected, boolean selectedAncestor) {
            this(selected, selectedAncestor, null);
            this.conditionResult = null;
        }
        
        public MenuItemState(boolean selected, boolean selectedAncestor, Boolean conditionResult) {
            this.selected = selected;
            this.selectedAncestor = selectedAncestor;
            this.conditionResult = conditionResult;
            this.internalMap.put("selected", selected);
            this.internalMap.put("selectedAncestor", selectedAncestor);
            this.internalMap.put("selectedOrAncestor", (selected||selectedAncestor));
        }
        
        public static MenuItemState fromCurrent(ModelMenuItem menuItem, Map<String, Object> context) {
            return fromCurrent(menuItem, context, MenuRenderState.retrieve(context));
        }
        
        public static MenuItemState fromCurrent(ModelMenuItem menuItem, Map<String, Object> context, 
                MenuRenderState renderState) {
            MenuAndItem selectedMenuAndItem = renderState.getSelectedMenuAndItem(context);
            ModelMenuItem selectedMenuItem = selectedMenuAndItem.getMenuItem();
            ModelSubMenu selectedSubMenu = selectedMenuAndItem.getSubMenu();
            boolean selected = menuItem.isSame(selectedMenuItem);
            boolean selectedAncestor = !selected && menuItem.isAncestorOf(selectedSubMenu);
            return new MenuItemState(selected, selectedAncestor);
        }
        
        public boolean isSelected() {
            return selected;
        }
        
        public boolean isSelectedAncestor() {
            return selectedAncestor;
        }
        
        public boolean isSelectedOrAncestor() {
            return (selected || selectedAncestor);
        }

        public Boolean getConditionResult() {
            return conditionResult;
        }

        public void setConditionResult(Boolean conditionResult) {
            this.conditionResult = conditionResult;
            this.internalMap.put("conditionResult", conditionResult);
        }
    }

}