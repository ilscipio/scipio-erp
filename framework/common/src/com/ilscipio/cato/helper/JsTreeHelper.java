package com.ilscipio.cato.helper;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

import org.ofbiz.base.lang.JSON;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilValidate;

/**
 * Provides several utilities for handling jsTree objects (data, settings,
 * etc.). Compatible with jsTree version 3.x.x
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

    public static abstract class JsTreePlugin extends HashMap<String, Object> {
        private static final long serialVersionUID = 4248393333916299308L;

        public static JsTreePlugin newInstance(String type) {
            switch (type) {
            case "changed":
                return new JsTreeChangedPlugin();
            case "checkbox":
                return new JsTreeCheckboxPlugin();
            case "conditionalselect":
                return new JsTreeConditionalSelectPlugin();
            case "contextmenu":
                return new JsTreeContextMenuPlugin();
            case "dnd":
                return new JsTreeDndPlugin();
            case "massload":
                return new JsTreeMassLoadPlugin();
            case "search":
                return new JsTreeSearchPlugin();
            case "sort":
                return new JsTreeSortPlugin();
            case "state":
                return new JsTreeStatePlugin();
            case "types":
                return new JsTreeTypesPlugin();
            case "unique":
                return new JsTreeUniquePlugin();
            case "wholerow":
                return new JsTreeWholeRowPlugin();
            default:
                return null;
            }

        }

        public abstract String pluginName();

        public static class JsTreeChangedPlugin extends JsTreePlugin {
            private static final long serialVersionUID = 5601692536800453039L;
            private static final String PLUGIN_NAME = "changed";

            public String pluginName() {
                return PLUGIN_NAME;
            }
        }

        public static class JsTreeCheckboxPlugin extends JsTreePlugin {
            private static final long serialVersionUID = -3879179716936704695L;
            private static final String PLUGIN_NAME = "checkbox";

            public String pluginName() {
                return PLUGIN_NAME;
            }

            public void setVisible(boolean visible) {
                put("visible", visible);
            }

            public void setThreeState(boolean threeState) {
                put("threeState", threeState);
            }

            public void setWholeNode(boolean wholeNode) {
                put("whole_node", wholeNode);
            }

            public void setKeepSelectedStyle(boolean keepSelectedStyle) {
                put("keep_selected_style", keepSelectedStyle);
            }

            public void setCascade(String cascade) {
                put("cascade", cascade);
            }

            public void setTieSelection(boolean tieSelection) {
                put("tie_selection", tieSelection);
            }
        }

        public static class JsTreeConditionalSelectPlugin extends JsTreePlugin {
            private static final long serialVersionUID = -6731206801768339246L;
            private static final String PLUGIN_NAME = "conditional_select";

            public String pluginName() {
                return PLUGIN_NAME;
            }

        }

        public static class JsTreeContextMenuPlugin extends JsTreePlugin {
            private static final long serialVersionUID = -4138062928604616321L;
            private static final String PLUGIN_NAME = "contextmenu";

            public String pluginName() {
                return PLUGIN_NAME;
            }

        }

        public static class JsTreeDndPlugin extends JsTreePlugin {
            private static final long serialVersionUID = -9112055711238818500L;
            private static final String PLUGIN_NAME = "dnd";

            public String pluginName() {
                return PLUGIN_NAME;
            }

        }

        public static class JsTreeMassLoadPlugin extends JsTreePlugin {
            private static final long serialVersionUID = -4221852625063222557L;
            private static final String PLUGIN_NAME = "massload";

            public String pluginName() {
                return PLUGIN_NAME;
            }

        }

        public static class JsTreeSearchPlugin extends JsTreePlugin {
            private static final long serialVersionUID = 633918483044354642L;
            private static final String PLUGIN_NAME = "search";

            public String pluginName() {
                return PLUGIN_NAME;
            }

        }

        public static class JsTreeSortPlugin extends JsTreePlugin {
            private static final long serialVersionUID = -6591298020312508748L;
            private static final String PLUGIN_NAME = "sort";

            public String pluginName() {
                return PLUGIN_NAME;
            }

        }

        public static class JsTreeStatePlugin extends JsTreePlugin {
            private static final long serialVersionUID = 3547524113762626757L;
            private static final String PLUGIN_NAME = "state";

            public String pluginName() {
                return PLUGIN_NAME;
            }

        }

        public static class JsTreeTypesPlugin extends JsTreePlugin {
            private static final long serialVersionUID = -100592862633027962L;
            private static final String PLUGIN_NAME = "types";
            public static final String ROOT_NODE = "#";
            public static final String DEFAULT_NODE = "default";

            public String pluginName() {
                return PLUGIN_NAME;
            }

            public void addType(String type, JsTreeType jsTreeType) {
                put(type, jsTreeType);
            }

            public static class JsTreeType extends HashMap<String, Object> {
                private static final long serialVersionUID = 2385436629068719748L;

                public void setMaxChildren(int maxChildren) {
                    put("max_children", maxChildren);
                }

                public void setMaxDepth(int maxDepth) {
                    put("max_depth", maxDepth);
                }

                public void setValidChildren(String validChildren) {
                    put("valid_children", validChildren);
                }

                public void setIcon(String icon) {
                    put("icon", icon);
                }

            }

        }

        public static class JsTreeUniquePlugin extends JsTreePlugin {
            private static final long serialVersionUID = 3876851222065997104L;
            private static final String PLUGIN_NAME = "unique";

            public String pluginName() {
                return PLUGIN_NAME;
            }

        }

        public static class JsTreeWholeRowPlugin extends JsTreePlugin {
            private static final long serialVersionUID = 2528095351504191020L;
            private static final String PLUGIN_NAME = "wholerow";

            public String pluginName() {
                return PLUGIN_NAME;
            }

        }

    }

    public static class JsTreePluginList extends ArrayList<JsTreePlugin> {
        private static String[] VALID_PLUGINS = { "changed", "checkbox", "conditionalselect", "contextmenu", "dnd", "massload", "search", "sort", "state",
                "types", "unique", "wholerow" };

        public void add(String p) {
            String[] plugins = p.split(";");
            for (String plugin : plugins) {
                if (Arrays.asList(VALID_PLUGINS).contains(plugin) && !this.contains(plugin)) {
                    add(JsTreePlugin.newInstance(plugin));
                }
            }

        }

        public boolean addPlugin(String plugin) {
            if (Arrays.asList(VALID_PLUGINS).contains(plugin) && !this.contains(plugin)) {
                return add(JsTreePlugin.newInstance(plugin));
            }
            return false;
        }

    }

}