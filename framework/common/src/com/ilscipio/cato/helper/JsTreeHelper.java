package com.ilscipio.cato.helper;

import java.io.IOException;
import java.util.HashMap;
import java.util.List;

import org.ofbiz.base.lang.JSON;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilValidate;

/**
 * Provides several utilities for handling jsTree objects (data, settings, etc.). Compatible
 * with jsTree version 3.x.x
 * 
 * @author jsoto
 * 
 */
public class JsTreeHelper {

    public static String buildTreeData(List<JsTreeDataItem> treeDataList) {
        StringBuffer buffer = new StringBuffer();
        JSON json;
        try {
            json = JSON.from(treeDataList);
            buffer.append(json.toString());
            Debug.log("json ==========> " + buffer.toString());
        } catch (IOException e) {
            Debug.logError(e, "JsTreeHelper");
        }
        return buffer.toString();
    }

    public static class JsTreeDataItem extends HashMap<String, Object> {
        private static final long serialVersionUID = -660269373973470543L;

        public JsTreeDataItem(String id, String text, String icon, JsTreeDataItemState state, List<JsTreeDataItem> children) {
            if (UtilValidate.isNotEmpty(id))
                put("id", id);
            if (UtilValidate.isNotEmpty(text))
                put("text", text);
            if (UtilValidate.isNotEmpty(state))
                put("state", state);
            if (UtilValidate.isNotEmpty(icon))
                put("icon", icon);
            if (UtilValidate.isNotEmpty(children))
                put("children", children);
        }

        public String getId() {
            return (String) get("id");
        }

        public String getText() {
            return (String) get("text");
        }

        public String getIcon() {
            return (String) get("icon");
        }

        public JsTreeDataItemState getState() {
            return (JsTreeDataItemState) get("state");
        }

        public List<JsTreeDataItem> getChildren() {
            return (List<JsTreeDataItem>) get("children");
        }

        public void setId(String id) {
            put("id", id);
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

        public void setChildren(List<JsTreeDataItem> children) {
            put("children", children);
        }

        public static class JsTreeDataItemState extends HashMap<String, Boolean> {
            private static final long serialVersionUID = -3407209672564973008L;

            public JsTreeDataItemState(boolean opened, boolean selected) {
                put("opened", opened);
                put("selected", selected);
            }

            public boolean isOpened() {
                return get("opened");
            }

            public boolean isSelected() {
                return get("selected");
            }

            public void setOpened(boolean opened) {
                put("opened", opened);
            }

            public void setSelected(boolean selected) {
                put("selected", selected);
            }

        }
    }
}