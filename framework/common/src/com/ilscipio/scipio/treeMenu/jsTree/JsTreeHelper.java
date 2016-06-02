package com.ilscipio.scipio.treeMenu.jsTree;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilValidate;

import javolution.util.FastMap;

/**
 * Provides several utilities for handling jsTree objects (data, settings,
 * etc.). Compatible with jsTree version 3.x.x
 * 
 * @author jsoto
 * 
 */
public class JsTreeHelper extends ArrayList<JsTreeDataItem> {
    private static final long serialVersionUID = -8201109323209706803L;
    private static String JSTREE_FIELD_ID_SEPARATOR = "_";

    private static String module = "JsTreeHelper";

    private Map<String, Integer> sameIdDataItemsMap = FastMap.newInstance();

    @Override
    public boolean add(JsTreeDataItem e) {
        boolean r = super.add(e);
        if (r) {
            findRepeatedDataItems();
            Debug.logInfo("Total items repeated ============> " + sameIdDataItemsMap.keySet().size(), module);
            if (sameIdDataItemsMap.containsKey(e.getId()))
                updateTreeDataItemsIdAndParentReference(e.getId());
        }
        return r;
    }

    @Override
    public void add(int index, JsTreeDataItem element) {
        super.add(index, element);
        findRepeatedDataItems();
        Debug.logInfo("Total items repeated ============> " + sameIdDataItemsMap.keySet().size(), module);
        if (sameIdDataItemsMap.containsKey(element.getId()))
            updateTreeDataItemsIdAndParentReference(element.getId());
    }

    @Override
    public boolean addAll(Collection<? extends JsTreeDataItem> c) {
        boolean r = super.addAll(c);
        if (r) {
            findRepeatedDataItems();
            Debug.logInfo("Total items repeated ============> " + sameIdDataItemsMap.keySet().size(), module);
            for (String idKey : sameIdDataItemsMap.keySet()) {
                // Debug.logInfo("updating id ============> " + idKey + " " +
                // sameIdDataItemsMap.get(idKey) + " times", module);
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
            Debug.logInfo("Total items repeated ============> " + sameIdDataItemsMap.keySet().size(), module);
            for (String idKey : sameIdDataItemsMap.keySet()) {
                // Debug.logInfo("updating id ============> " + idKey + " " +
                // sameIdDataItemsMap.get(idKey) + " times", module);
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
                // Debug.logInfo(
                // "updating id ============> " + item.getOriginalId() + "
                // remaining items with the same id to be updated =============>
                // " + idCount, module);
            } else if (item.getOriginalId().equals(id) && idCount < 0) {
                Debug.log("Reached maximum number of replacements for id ====> " + id);
            }

            // Update parent
            if (item.getParent().equals(id)) {
                item.setParent(item.getParent() + JSTREE_FIELD_ID_SEPARATOR + idCount);
                // Debug.logInfo("updating parent ============> " +
                // item.getParent() + " remaining items with the same parent to
                // be updated =============> "
                // + idCount, module);
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