package com.ilscipio.scipio.treeMenu.jsTree;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

/**
 * 
 * @author jsoto
 *
 */
@SuppressWarnings("serial")
public abstract class JsTreePlugin extends HashMap<String, Object> {

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

        public JsTreeTypesPlugin(String[] types, JsTreeType... jsTreeType) throws Exception {
            this(Arrays.asList(types), jsTreeType);
        }

        public JsTreeTypesPlugin(List<String> types, JsTreeType... jsTreeType) throws Exception {
            if (types.size() != jsTreeType.length)
                throw new Exception("Odd arrays have been passed. The number of types and and jsTreeTypes must be the same.");
            for (int i = 0; i < types.size(); i++)
                addType(types.get(i), jsTreeType[i]);
        }

        public JsTreeTypesPlugin() {
        }

        public void addType(String type, JsTreeType jsTreeType) {
            put(type, jsTreeType);
        }

        public static class JsTreeType extends HashMap<String, Object> {
            private static final long serialVersionUID = 2385436629068719748L;

            public JsTreeType(Integer maxChildren, Integer maxDepth, List<String> validChildren, String icon) {
                if (maxChildren == null)
                    maxChildren = -1;
                setMaxChildren(maxChildren);
                if (maxDepth == null)
                    maxDepth = -1;
                setMaxDepth(maxDepth);
                setValidChildren(validChildren);
                setIcon(icon);
            }

            public JsTreeType() {
                setMaxChildren(-1);
                setMaxDepth(-1);
                setValidChildren(null);
                setIcon(null);
            }

            public void setMaxChildren(int maxChildren) {
                put("max_children", maxChildren);
            }

            public void setMaxDepth(int maxDepth) {
                put("max_depth", maxDepth);
            }

            public void setValidChildren(List<String> validChildren) {
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
