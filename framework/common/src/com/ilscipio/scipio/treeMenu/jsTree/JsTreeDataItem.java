package com.ilscipio.scipio.treeMenu.jsTree;

import java.util.HashMap;
import java.util.Map;

import org.ofbiz.base.util.UtilValidate;

import com.ilscipio.scipio.treeMenu.TreeDataItem;

/**
 * 
 * @author jsoto
 *
 */
@SuppressWarnings("serial")
public class JsTreeDataItem extends HashMap<String, Object> implements TreeDataItem {

    private String originalId;

    public JsTreeDataItem(String id, String text, String icon, JsTreeDataItemState state, String parent) {
        if (UtilValidate.isNotEmpty(id))
            put("id", id);
        if (UtilValidate.isNotEmpty(text))
            put("text", text);
        if (UtilValidate.isNotEmpty(state))
            put("state", state);
        if (UtilValidate.isNotEmpty(icon))
            put("icon", icon);
        if (UtilValidate.isNotEmpty(parent))
            put("parent", parent);
        else
            put("parent", "#");

        this.originalId = id;
        Map<String, String> liAttrValues = new HashMap<>();
        liAttrValues.put("original_id", originalId);
        put("li_attr", liAttrValues);
    }

    @Override
    public void setId(String id) {
        put("id", id);
    }

    @Override
    public String getId() {
        return (String) this.get("id");
    }

    public void setText(String text) {
        put("text", text);
    }

    public void setIcon(String icon) {
        put("icon", icon);
    }

    public void setState(JsTreeDataItemState state) {
        put("state", state);
    }

    public void setParent(String parent) {
        put("parent", parent);
    }

    public String getParent() {
        return (String) get("parent");
    }

    public void setType(String type) {
        put("type", type);
    }

    public String getOriginalId() {
        return originalId;
    }

    protected void setOriginalId(String originalId) {
        this.originalId = originalId;
    }

    public static class JsTreeDataItemState extends HashMap<String, Boolean> {
        private static final long serialVersionUID = -3407209672564973008L;

        public JsTreeDataItemState(boolean opened, boolean selected) {
            put("opened", opened);
            put("selected", selected);
        }

        public JsTreeDataItemState(boolean opened, boolean selected, boolean disabled) {
            this(opened, selected);
            put("disabled", disabled);
        }
        
        public JsTreeDataItemState(Map<String, Object> stateMap) {
            put("opened", (Boolean) stateMap.get("opened"));
            put("selected", (Boolean) stateMap.get("selected"));
            put("disabled", (Boolean) stateMap.get("disabled"));
        }

        public boolean isOpened() {
            return get("opened");
        }

        public boolean isSelected() {
            return get("selected");
        }

        public boolean isDisabled() {
            return get("disabled");
        }

        public void setOpened(boolean opened) {
            put("opened", opened);
        }

        public void setSelected(boolean selected) {
            put("selected", selected);
        }

        public void setDisabled(boolean disabled) {
            put("disabled", disabled);
        }
    }

    @Override
    public boolean equals(Object o) {
        return this == o;
    }

}
