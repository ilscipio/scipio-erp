package com.ilscipio.cato.treeMenu.jsTree;

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

    private Map<String, Integer> sameIdDataItemsMap = FastMap.newInstance();

    @Override
    public boolean add(JsTreeDataItem e) {
        boolean r = super.add(e);
        if (r) {
            findRepeatedDataItems();
            if (sameIdDataItemsMap.containsKey(e.getId()))
                updateTreeDataItemsIdAndParentReference(e);
        }
        return r;
    }

    @Override
    public void add(int index, JsTreeDataItem element) {
        super.add(index, element);
        findRepeatedDataItems();
        if (sameIdDataItemsMap.containsKey(element.getId()))
            updateTreeDataItemsIdAndParentReference(element);
    }

    @Override
    public boolean addAll(Collection<? extends JsTreeDataItem> c) {
        boolean r = super.addAll(c);
        if (r) {
            findRepeatedDataItems();
            for (JsTreeDataItem item : this) {
                if (sameIdDataItemsMap.containsKey(item.getId()))
                    updateTreeDataItemsIdAndParentReference(item);
            }
        }
        return r;
    }

    @Override
    public boolean addAll(int index, Collection<? extends JsTreeDataItem> c) {
        boolean r = super.addAll(index, c);
        if (r) {
            findRepeatedDataItems();
            for (JsTreeDataItem item : c) {
                if (sameIdDataItemsMap.containsKey(item.getId()))
                    updateTreeDataItemsIdAndParentReference(item);
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

    private void updateTreeDataItemsIdAndParentReference(JsTreeDataItem i) {
        Debug.log("total items repeated ============> " + sameIdDataItemsMap.keySet().size());
        // Update id
        String idKeyRepeated = null;
        for (String idKey : sameIdDataItemsMap.keySet()) {
            Debug.log("id ==========> " + idKey + "  times repeated ============> " + sameIdDataItemsMap.get(idKey));
            int count = sameIdDataItemsMap.get(idKey);
            if (i.getOriginalId().equals(idKey) && count >= 0) {
                i.setId(i.getOriginalId() + JSTREE_FIELD_ID_SEPARATOR + count);
                if (UtilValidate.isNotEmpty(idKeyRepeated))
                    idKeyRepeated = i.getOriginalId();
            } else if (count < 0) {
                Debug.log("Houston, we got a problem");
            }
        }

        // Update parent references
        int repeatedTimes = sameIdDataItemsMap.get(idKeyRepeated);
        for (JsTreeDataItem x : this) {
            if (x.getParent().equals(idKeyRepeated)) {
                x.setParent(x.getParent() + JSTREE_FIELD_ID_SEPARATOR + repeatedTimes);
                repeatedTimes--;
            }
        }
    }

    private boolean hasTreeDataItem(JsTreeDataItem item) {
        for (JsTreeDataItem i : this) {
            if (i.getOriginalId().equals(item.getOriginalId()) && !i.equals(item)) {
                return true;
            }
        }
        return false;
    }

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

}