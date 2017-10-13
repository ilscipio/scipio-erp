package com.ilscipio.scipio.treeMenu.jsTree;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import org.ofbiz.base.util.UtilValidate;

/**
 * Provides several utilities for handling jsTree objects (data, settings,
 * etc.). Compatible with jsTree version 3.x.x
 * 
 * @author jsoto
 * 
 */
@SuppressWarnings("serial")
public class JsTreeHelper extends ArrayList<JsTreeDataItem> {
    private static final String module = JsTreeHelper.class.getName();

    private static final String JSTREE_FIELD_ID_SEPARATOR = "_";

    private Map<String, Integer> sameIdDataItemsMap = new HashMap<>();

    @Override
    public boolean add(JsTreeDataItem e) {
        boolean r = super.add(e);
        if (r) {
            findRepeatedDataItems();
            if (sameIdDataItemsMap.containsKey(e.getId()))
                updateTreeDataItemsIdAndParentReference(e.getId());
        }
        return r;
    }

    @Override
    public void add(int index, JsTreeDataItem element) {
        super.add(index, element);
        findRepeatedDataItems();
        if (sameIdDataItemsMap.containsKey(element.getId()))
            updateTreeDataItemsIdAndParentReference(element.getId());
    }

    @Override
    public boolean addAll(Collection<? extends JsTreeDataItem> c) {
        boolean r = super.addAll(c);
        if (r) {
            findRepeatedDataItems();
            for (String idKey : sameIdDataItemsMap.keySet()) {
                updateTreeDataItemsIdAndParentReference(idKey);
            }
        }
        return r;
    }

    @Override
    public boolean addAll(int index, Collection<? extends JsTreeDataItem> c) {
        boolean r = super.addAll(index, c);
        if (r) {
            findRepeatedDataItems();
            for (String idKey : sameIdDataItemsMap.keySet()) {
                updateTreeDataItemsIdAndParentReference(idKey);
            }
        }
        return r;
    }

    /**
     * 
     * @author jsoto
     *
     */
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

    /**
     * Updates the dataItemids that matches the id passed and as well as all the
     * parent references
     * 
     * @param id
     */
    private void updateTreeDataItemsIdAndParentReference(String id) {
        int idCount = sameIdDataItemsMap.get(id);
        for (JsTreeDataItem item : this) {
            // Update id
            if (item.getOriginalId().equals(id) && idCount >= 0) {
                item.setId(item.getOriginalId() + JSTREE_FIELD_ID_SEPARATOR + idCount);
                idCount--;
            }
            // Update parent
            if (item.getParent().equals(id)) {
                item.setParent(item.getParent() + JSTREE_FIELD_ID_SEPARATOR + idCount);
            }
        }
    }

    /**
     * Finds all repeated elements, or basically the ones with the same id so
     * they can be treated later on
     */
    private void findRepeatedDataItems() {
        for (JsTreeDataItem dataItem : this) {
            Integer timesRepeated = 0;
            if (hasTreeDataItem(dataItem)) {
                if (sameIdDataItemsMap.containsKey(dataItem.getOriginalId()))
                    timesRepeated = sameIdDataItemsMap.get(dataItem.getOriginalId());
                timesRepeated++;
                sameIdDataItemsMap.put(dataItem.getOriginalId(), timesRepeated);
            }
        }
    }

    private boolean hasTreeDataItem(JsTreeDataItem item) {
        for (JsTreeDataItem i : this) {
            if (i.getOriginalId().equals(item.getOriginalId()) && !i.equals(item)) {
                if (UtilValidate.isEmpty(item.get("type")) || (UtilValidate.isNotEmpty(item.get("type")) && item.get("type").equals(i.get("type")))) {
                    return true;
                }
            }
        }
        return false;
    }

}