package com.ilscipio.cato.helper;

import java.io.IOException;
import java.util.HashMap;
import java.util.List;

import org.ofbiz.base.lang.JSON;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilValidate;

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
        private String id;
        private String text;
        private String icon;
        private JsTreeDataItemState state;
        private List<JsTreeDataItem> children;

        public JsTreeDataItem(String id, String text, String icon, JsTreeDataItemState state, List<JsTreeDataItem> children) {
            this.id = id;
            this.text = text;
            this.state = state;
            this.icon = icon;
            this.children = children;
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
            return id;
        }

        public String getText() {
            return text;
        }

        public String getIcon() {
            return icon;
        }

        public JsTreeDataItemState getState() {
            return state;
        }

        public List<JsTreeDataItem> getChildren() {
            return children;
        }

        public static class JsTreeDataItemState extends HashMap<String, Boolean> {
            private static final long serialVersionUID = -3407209672564973008L;
            private boolean opened;
            private boolean selected;

            public JsTreeDataItemState(boolean opened, boolean selected) {
                this.opened = opened;
                this.selected = selected;
                put("opened", opened);
                put("selected", selected);
            }

            public boolean isOpened() {
                return opened;
            }

            public boolean isSelected() {
                return selected;
            }

        }
    }
}