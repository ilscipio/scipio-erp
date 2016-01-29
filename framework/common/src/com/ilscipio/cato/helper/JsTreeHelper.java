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

    public static class JsTreeCore extends HashMap<String, Object> {
        private static final long serialVersionUID = 2371673939917629690L;

        public JsTreeCore(boolean multiple, HashMap<String, String> strings, Integer animation) {
            setMultiple(multiple);
            setStrings(strings);
            setAnimation(animation);
        }

        public void setMultiple(Boolean multiple) {
            put("multiple", multiple);
        }

        public void setStrings(HashMap<String, String> strings) {
            put("strings", strings);
        }

        public void setAnimation(Integer animation) {
            if (UtilValidate.isNotEmpty(animation))
                put("animation", animation);
        }

        public void setExpandSelectedOnload(Boolean expandSelectedOnload) {
            put("expand_selected_onload", expandSelectedOnload);
        }

        public void setWorker(Boolean worker) {
            put("worker", worker);
        }

        public void setForceText(Boolean forceText) {
            put("force_text", forceText);
        }

        public void setDblClickToggle(Boolean dblClickToggle) {
            put("dblclick_toggle", dblClickToggle);
        }

        public void setThemes(JsTreeTheme themes) {
            put("themes", themes);
        }

        public static class JsTreeTheme extends HashMap<String, Object> {
            private static final long serialVersionUID = -4086721432593844943L;

            public JsTreeTheme(String themeName, String themeUrl, String themeDir, boolean themeDots, boolean themeIcons, boolean themeStripes,
                    String themeVariant, boolean themeResponsive) {

                setThemeName(themeName);
                setThemeUrl(themeUrl);
                setThemeDir(themeDir);
                setThemeDots(themeDots);
                setThemeIcons(themeIcons);
                setThemeStripes(themeStripes);
                setThemeVariant(themeVariant);
                setThemeResponsive(themeResponsive);

            }

            public JsTreeTheme(String themeName, String themeUrl, String themeDir) {
                setThemeName(themeName);
                setThemeUrl(themeUrl);
                setThemeDir(themeDir);
            }

            public void setThemeName(String themeName) {
                if (UtilValidate.isEmpty(themeName))
                    themeName = String.valueOf(Boolean.FALSE);
                put("name", themeName);
            }

            public void setThemeUrl(String themeUrl) {
                if (UtilValidate.isEmpty(themeUrl))
                    themeUrl = String.valueOf(Boolean.FALSE);
                put("url", themeUrl);
            }

            public void setThemeDir(String themeDir) {
                put("dir", themeDir);
            }

            public void setThemeDots(boolean themeDots) {
                put("dots", themeDots);
            }

            public void setThemeIcons(boolean themeIcons) {
                put("icons", themeIcons);
            }

            public void setThemeStripes(boolean themeStripes) {
                put("stripes", themeStripes);
            }

            public void setThemeVariant(String themeVariant) {
                put("variant", themeVariant);
            }

            public void setThemeResponsive(boolean themeResponsive) {
                put("responsive", themeResponsive);
            }

        }

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
        
        public void setType(String type) {
            put("type", type);
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

    public static class JsTreeEvent extends HashMap<String, String> {
        private static final long serialVersionUID = 2708617525071551004L;
        public static final String JSTREE_EVENT = ".jstree";

        public static final String[] VALID_EVENTS = { "init", "loading", "loaded", "ready", "load_node", "load_all", "model", "redraw", "before_open",
                "open_node", "after_open", "close_node", "after_close", "open_all", "close_all", "enable_node", "disable_node", "show_node", "hide_all",
                "show_all", "activate_node", "hover_node", "dehover_node", "select_node", "changed", "deselect_node", "select_all", "deselect_all",
                "set_state", "refresh", "refresh_node", "set_text", "create_node", "rename_node", "delete_node", "move_node", "copy_node", "cut", "copy",
                "paste", "clear_buffer", "set_theme" };

        public JsTreeEvent(String event, String function) {
            addEvent(event, function);
        }

        public void addEvent(String event, String function, String... params) {
            put(event, function);
        }

        @Override
        public String put(String key, String value) {
            if (Arrays.asList(VALID_EVENTS).contains(key + JSTREE_EVENT) && !this.containsKey(key + JSTREE_EVENT)) {
                return super.put(key + JSTREE_EVENT, value);
            }
            return null;
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

            public JsTreeTypesPlugin(String[] types, JsTreeType... jsTreeType) throws Exception {
                this(Arrays.asList(types), jsTreeType);
            }

            public JsTreeTypesPlugin(List<String> types, JsTreeType... jsTreeType) throws Exception {
                if (types.size() != jsTreeType.length)
                    throw new Exception("Odd array sizes have been passed. The number of types and and jsTreeTypes must be the same.");
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

                public JsTreeType(Integer maxChildren, Integer maxDepth, String validChildren, String icon) {
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
        private static final long serialVersionUID = 2601099764369384246L;
        private static String[] VALID_PLUGINS = { "changed", "checkbox", "conditionalselect", "contextmenu", "dnd", "massload", "search", "sort", "state",
                "types", "unique", "wholerow" };

        public void add(String p) {
            String[] plugins = p.split(",");
            for (String plugin : plugins) {
                plugin = plugin.trim();
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