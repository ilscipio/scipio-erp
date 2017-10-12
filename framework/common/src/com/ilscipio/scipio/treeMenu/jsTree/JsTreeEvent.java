package com.ilscipio.scipio.treeMenu.jsTree;

import java.util.Arrays;
import java.util.HashMap;

/**
 * 
 * @author jsoto
 *
 */
@SuppressWarnings("serial")
public class JsTreeEvent extends HashMap<String, String> {
    public static final String JSTREE_EVENT = ".jstree";

    public static final String[] VALID_EVENTS = { "init", "loading", "loaded", "ready", "load_node", "load_all", "model", "redraw", "before_open", "open_node",
            "after_open", "close_node", "after_close", "open_all", "close_all", "enable_node", "disable_node", "show_node", "hide_all", "show_all",
            "activate_node", "hover_node", "dehover_node", "select_node", "changed", "deselect_node", "select_all", "deselect_all", "set_state", "refresh",
            "refresh_node", "set_text", "create_node", "rename_node", "delete_node", "move_node", "copy_node", "cut", "copy", "paste", "clear_buffer",
            "set_theme" };

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