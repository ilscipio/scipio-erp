package com.ilscipio.scipio.product.category;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

import com.ilscipio.scipio.product.category.CategoryWorker.TreeBuildOptions;
import com.ilscipio.scipio.treeMenu.TreeDataItem;
import com.ilscipio.scipio.treeMenu.jsTree.JsTreeDataItem;
import com.ilscipio.scipio.treeMenu.jsTree.JsTreeDataItem.JsTreeDataItemState;


/**
 * SCIPIO: Category services for novel/extra functionality.
 * <p>
 * Added 2017-10-12; some methods moved here from {@link org.ofbiz.product.category.CategoryServices}.
 * DEV NOTE: jsTree-related methods moved under a dedicated scipio package because they were
 * too specific functionality.
 */
public abstract class CategoryServices {

    //private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    protected CategoryServices() {
    }

    /**
     * SCIPIO: buildCatalogTree implementation (for jsTree).
     */
    public static Map<String, Object> buildCatalogTree(DispatchContext dctx, Map<String, ? extends Object> context) {
        Map<String, Object> result = ServiceUtil.returnSuccess();
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        String library = (String) context.get("library");
        String mode = (String) context.get("mode");
        String prodCatalogId = (String) context.get("prodCatalogId");

        Map<String, Object> state = UtilGenerics.checkMap(context.get("state"));
        boolean includeEmptyTop = Boolean.TRUE.equals(context.get("includeEmptyTop"));

        TreeBuildOptions treeBuildOpts = new TreeBuildOptions(context);
        Map<String, ? super GenericValue> categoryEntityOutMap = UtilGenerics.checkMap(context.get("categoryEntityOutMap"));

        List<TreeDataItem> resultList = new ArrayList<>();
        if (mode.equals("full")) {
            try {
                GenericValue productStoreCatalog = (GenericValue) context.get("productStoreCatalog");

                GenericValue catalog = EntityQuery.use(delegator).from("ProdCatalog").where("prodCatalogId", prodCatalogId).queryOne();
                List<GenericValue> prodCatalogCategories = EntityQuery.use(delegator).from("ProdCatalogCategory").where("prodCatalogId", prodCatalogId)
                        .filterByDate().queryList();
                boolean hasCategories = UtilValidate.isNotEmpty(prodCatalogCategories);
                if (includeEmptyTop || hasCategories) {

                    JsTreeDataItem dataItem = null;
                    if (library.equals("jsTree")) {
                        String nodeId = "catalog_" + prodCatalogId;
                        if (hasCategories) {
                            resultList.addAll(CategoryWorker.getTreeCategories(delegator, dispatcher, locale, prodCatalogCategories, nodeId, treeBuildOpts, categoryEntityOutMap));
                        }
                        Map<String, Object> effState = UtilMisc.toMap("opened", false, "selected", false);
                        if (state != null) {
                            effState.putAll(state);
                        }
                        dataItem = new JsTreeDataItem(nodeId, prodCatalogId, catalog.getString("catalogName"), "jstree-folder", new JsTreeDataItemState(effState),
                                null);
                        dataItem.setType("catalog");
                        treeBuildOpts.checkPutEntityDataField(dataItem, "prodCatalog", catalog);
                        treeBuildOpts.checkPutEntityDataField(dataItem, "productStoreCatalog", productStoreCatalog);
                        dataItem.put("isParent", hasCategories);
                    }

                    if (UtilValidate.isNotEmpty(dataItem))
                        resultList.add(dataItem);
                }

            } catch (GenericEntityException e) {
                return ServiceUtil.returnError(e.getMessage());
            } catch (GenericServiceException e) {
                return ServiceUtil.returnError(e.getMessage());
            }
        } else if (mode.equals("category")) {
            /*
             * TODO: Complete for other modes
             */
        } else if (mode.equals("product")) {
            /*
             * TODO: Complete for other modes
             */
        }

        result.put("treeList", resultList);
        result.put("categoryEntityOutMap", categoryEntityOutMap);
        return result;
    }
}
