package com.ilscipio.scipio.solr;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.lang.StringUtils;
import org.apache.solr.client.solrj.SolrQuery;
import org.apache.solr.client.solrj.SolrRequest.METHOD;
import org.apache.solr.client.solrj.impl.HttpSolrClient;
import org.apache.solr.client.solrj.response.QueryResponse;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericDelegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.product.catalog.CatalogWorker;
import org.ofbiz.service.DispatchContext;

/**
 * Product category util class for solr.
 * TODO: some of these method belong in non-solr classes
 */
public abstract class SolrCategoryUtil {
    
    public static final String module = SolrCategoryUtil.class.getName();

    /**
     * Gets catalog IDs for specified product category.
     * <p>
     * This method is a supplement to CatalogWorker methods.
     */
    static List<String> getCatalogIdsByCategoryId(Delegator delegator, String productCategoryId, boolean useCache) {
        List<GenericValue> catalogs = getProdCatalogCategoryByCategoryId(delegator, productCategoryId, useCache);
        return getStringFieldList(catalogs, "prodCatalogId");
    }
    
    static List<String> getCatalogIdsByCategoryId(Delegator delegator, String productCategoryId) {
        return getCatalogIdsByCategoryId(delegator, productCategoryId, false); // legacy
    }
    
    static List<GenericValue> getProdCatalogCategoryByCategoryId(Delegator delegator, String productCategoryId, boolean useCache) {
        List<GenericValue> catalogs = null;
        try {
            EntityCondition condition = EntityCondition.makeCondition(UtilMisc.toMap("productCategoryId", productCategoryId));
            condition = EntityCondition.makeCondition(condition, EntityOperator.AND, EntityUtil.getFilterByDateExpr());
            catalogs = delegator.findList("ProdCatalogCategory", condition, null, UtilMisc.toList("sequenceNum"), null, useCache);
        } catch (GenericEntityException e) {
            Debug.logError(e, "Solr: Error looking up catalogs for productCategoryId: " + productCategoryId, module);
            catalogs = new ArrayList<>();
        }
        return catalogs;
    }
    
    static Map<String, List<String>> getCatalogIdsByCategoryIdMap(Delegator delegator, List<String> categoryIds, boolean useCache) {
        Map<String, List<String>> map = new HashMap<>();
        if (categoryIds == null) return map;
        for(String categoryId : categoryIds) {
            map.put(categoryId, getStringFieldList(getProdCatalogCategoryByCategoryId(delegator, categoryId, useCache), "prodCatalogId"));
        }
        return map;
    }
    
    // FIXME: doesn't belong in solr
    static List<String> getStringFieldList(List<GenericValue> values, String fieldName) {
        List<String> fieldList;
        if (UtilValidate.isNotEmpty(values)) {
            fieldList = new ArrayList<>(values.size());
            for (GenericValue c : values) {
                fieldList.add(c.getString(fieldName));
            }
        } else {
            fieldList = new ArrayList<>();
        }
        return fieldList;
    }
    
    static void addAllStringFieldList(Collection<String> out, List<GenericValue> values, String fieldName) {
        if (values != null) {
            for (GenericValue c : values) {
                out.add(c.getString(fieldName));
            }
        }
    }
    
    /**
     * Best-effort.
     */
    static List<GenericValue> getProductStoresFromCatalogIds(Delegator delegator, Collection<String> catalogIds, boolean useCache) {
        List<GenericValue> stores = new ArrayList<>();
        Set<String> storeIds = new HashSet<>();
        for(String catalogId : catalogIds) {
            try {
                EntityCondition condition = EntityCondition.makeCondition(UtilMisc.toMap("prodCatalogId", catalogId));
                condition = EntityCondition.makeCondition(condition, EntityOperator.AND, EntityUtil.getFilterByDateExpr());
                List<GenericValue> productStoreCatalogs = delegator.findList("ProductStoreCatalog", condition, null, null, null, useCache);
                for(GenericValue productStoreCatalog : productStoreCatalogs) {
                    if (!storeIds.contains(productStoreCatalog.getString("productStoreId"))) {
                        stores.add(productStoreCatalog.getRelatedOne("ProductStore", useCache));
                        storeIds.add(productStoreCatalog.getString("productStoreId"));
                    }
                }
            } catch(Exception e) {
                Debug.logError(e, "Solr: Error looking up ProductStore for catalogId: " + catalogId, module);
            }
        }
        return stores;
    }
    
    public static List<List<String>> getCategoryTrail(String productCategoryId, DispatchContext dctx) {
        GenericDelegator delegator = (GenericDelegator) dctx.getDelegator();
        List<List<String>> trailElements = new ArrayList<>();
        // 2016-03-22: don't need a loop here due to change below
        //String parentProductCategoryId = productCategoryId;
        //while (UtilValidate.isNotEmpty(parentProductCategoryId)) {
            // find product category rollup
        try {
            List<EntityCondition> rolllupConds = new ArrayList<>();
            //rolllupConds.add(EntityCondition.makeCondition("productCategoryId", parentProductCategoryId));
            rolllupConds.add(EntityCondition.makeCondition("productCategoryId", productCategoryId));
            rolllupConds.add(EntityUtil.getFilterByDateExpr());
            // NOTE: Can't filter on sequenceNum because it only makes sense if querying by parentProductCategoryId
            List<String> orderBy = UtilMisc.toList("-fromDate");
            List<GenericValue> productCategoryRollups = delegator.findList("ProductCategoryRollup", EntityCondition.makeCondition(rolllupConds), null, orderBy, null, true);
            if (UtilValidate.isNotEmpty(productCategoryRollups)) {
                /* 2016-03-22: This does not work properly and creates invalid trails.
                 * Instead, use recursion.
                List<List<String>> trailElementsAux = new ArrayList<>();
                trailElementsAux.addAll(trailElements);
                // add only categories that belong to the top category to trail
                for (GenericValue productCategoryRollup : productCategoryRollups) {
                    String trailCategoryId = productCategoryRollup.getString("parentProductCategoryId");
                    parentProductCategoryId = trailCategoryId;
                    List<String> trailElement = new ArrayList<>();
                    if (!trailElements.isEmpty()) {
                        for (List<String> trailList : trailElementsAux) {
                            trailElement.add(trailCategoryId);
                            trailElement.addAll(trailList);
                            trailElements.remove(trailList);
                            trailElements.add(trailElement);
                        }
                    } else {
                        trailElement.add(trailCategoryId);
                        trailElement.add(productCategoryId);
                        trailElements.add(trailElement);
                    }
                }
                */
                
                // For each parent cat, get its trails recursively and add our own
                for (GenericValue productCategoryRollup : productCategoryRollups) {
                    String parentProductCategoryId = productCategoryRollup.getString("parentProductCategoryId");
                    List<List<String>> parentTrails = getCategoryTrail(parentProductCategoryId, dctx);
                    for (List<String> trail : parentTrails) {
                        // WARN: modifying the parent trail in-place for speed
                        trail.add(productCategoryId);
                        trailElements.add(trail);
                    }
                }
            }
            //} else {
            //    parentProductCategoryId = null;
            //}

        } catch (GenericEntityException e) {
            Debug.logError(e, "Solr: Cannot generate trail from product category; SOLR query or data may be incomplete!", module);
        }
        //}
        if (trailElements.isEmpty()) {
            List<String> trailElement = new ArrayList<>();
            trailElement.add(productCategoryId);
            trailElements.add(trailElement);
        }
        return trailElements;
    }
    
    /**
     * Returns categoryName with trail.
     */
    public static String getCategoryNameWithTrail(String productCategoryId, String catalogId, DispatchContext dctx, List<String> currentTrail) {
        return getCategoryNameWithTrail(productCategoryId, catalogId, true,  dctx, currentTrail);
    }
    
    /**
     * Returns categoryName with trail.
     * <p>
     * 2016-03-22: This now accepts a currentTrail needed because categories may have multiple trails.
     * It is checked to help select the most appropriate of the multiple trails to return.
     * If not specified, the first trail is returned;
     * CURRENTLY, it works like a hint rather than exact match - if no perfect match, returns the closest one.
     * FIXME?: it's possible we only want exact matches (exact) or matches containing the full currentTrail (containsFullTrail),
     * but I think it's safer for now to return closest-match.
     * <p>
     * NOTE: currentTrail should NOT contain the "TOP" category or the path length as first element.
     * Can be gotten using {@link org.ofbiz.product.category.CategoryWorker#getCategoryPathFromTrailAsList}.
     */
    public static String getCategoryNameWithTrail(String productCategoryId, String catalogId, Boolean showDepth, DispatchContext dctx, List<String> currentTrail) {
        List<List<String>> trailElements = SolrCategoryUtil.getCategoryTrail(productCategoryId, dctx);
        StringBuilder catMember = new StringBuilder();
        String cm = "";
        int i = 0;
        // 2016-03-22: This loop breaks because a category can have multiple trails.
        // WORKAROUND: use a current trail hint, or if none provided, use the first trail returned.
        //for (List<String> trailElement : trailElements) {
        if (!trailElements.isEmpty()) {
            List<String> trailElement = null;
            if (UtilValidate.isNotEmpty(currentTrail)) {
                // FIXME?: currently works like a hint, don't force exact matching
                trailElement = findBestTrailMatch(trailElements, currentTrail, false, false);
                if (trailElement == null) {
                    trailElement = getBestDefaultTrail(catalogId, dctx, trailElements);
                }
            } else {
                trailElement = getBestDefaultTrail(catalogId, dctx, trailElements);
            }
            if (trailElement != null) {
                for (Iterator<String> trailIter = trailElement.iterator(); trailIter.hasNext();) {
                    String trailString = (String) trailIter.next();
                    if (catMember.length() > 0){
                        catMember.append("/");
                        i++;
                    }
                    
                    catMember.append(trailString);
                }
            }
        }
        
        if (catMember.length() == 0){catMember.append(productCategoryId);}
        
        if(showDepth) {
            cm = i +"/"+ catMember.toString();
        } else {
            cm = catMember.toString();
        }
        //Debug.logInfo("Solr: getCategoryNameWithTrail: catMember: " + cm,module);
        return cm;
    }

    /**
     * Returns categoryName with trail.
     * @deprecated You should usually call the overload
     * with currentTrail instead {@link #getCategoryNameWithTrail(String, DispatchContext, List)}.
     */
    @Deprecated
    public static String getCategoryNameWithTrail(String productCategoryId, DispatchContext dctx) {
        return getCategoryNameWithTrail(productCategoryId, null, true,  dctx, null);
    }

    /**
     * Returns categoryName with trail.
     * @deprecated You should usually call the overload
     * with currentTrail instead {@link #getCategoryNameWithTrail(String, Boolean, DispatchContext, List)}.
     */
    @Deprecated
    public static String getCategoryNameWithTrail(String productCategoryId, Boolean showDepth, DispatchContext dctx) {
        return getCategoryNameWithTrail(productCategoryId, null, showDepth, dctx, null);
    }

    /**
     * Returns the most appropriate default trail.
     * <p>
     * If there is more than one, it will try to pick one from a default browse root.
     */
    public static List<String> getBestDefaultTrail(String catalogId, DispatchContext dctx, List<List<String>> trails) {
        if (trails == null || trails.isEmpty()) {
            return null;
        } else if (trails.size() == 1) {
            return trails.get(0);
        } else if (catalogId == null || catalogId.isEmpty()) {
            return trails.get(0);
        } else {
            List<String> best = null;
            Integer bestIndex = null;
            
            List<GenericValue> topCats = CatalogWorker.getProdCatalogCategories(dctx.getDelegator(), catalogId, "PCCT_BROWSE_ROOT");
            
            for(List<String> trail : trails) {
                if (trail != null && !trail.isEmpty()) {
                    String catId = trail.get(0);
                    int catIndex = 0;
                    for(GenericValue cat : topCats) {
                        if (cat.getString("productCategoryId").equals(catId)) {
                            // the topCats were sorted by sequenceNum, so lower index is best
                            if (best == null) {
                                best = trail;
                                bestIndex = catIndex;
                            } else {
                                if (catIndex < bestIndex) {
                                    best = trail;
                                    bestIndex = catIndex;
                                }
                            }
                            break;
                        } else {
                            catIndex++;
                        }
                    }
                }
            }
            if (best == null) {
                best = trails.get(0);
            }
            return best;
        }
    }
    
    
    
    /**
     * Finds best trail match.
     * <p>
     * If containFullTrail true, returns null if the best trail match does not fully contain the match trail;
     * if false, this will return the longest-path match found.
     * If exact true, has to match exactly.
     */
    public static List<String> findBestTrailMatch(List<List<String>> trails, List<String> matchTrail, boolean containFullTrail, boolean exact) {
        List<String> best = null;
        int partMatches = 0;
        
        for(List<String> candidateTrail : trails) {
            int candidatePartMatches = 0;
            Iterator<String> candidateIt = candidateTrail.iterator();
            Iterator<String> matchIt = matchTrail.iterator();
            while(candidateIt.hasNext() && matchIt.hasNext()) {
                String candidatePart = candidateIt.next();
                String matchPart = matchIt.next();
                if (candidatePart.equals(matchPart)) {
                    candidatePartMatches++;
                } else {
                    break;
                }
            }
            
            if (candidatePartMatches == matchTrail.size() && matchTrail.size() == candidateTrail.size()) {
                // Found exact match, return it right away as shortcut
                return candidateTrail;
            } else {
                if (candidatePartMatches > partMatches) {
                    partMatches = candidatePartMatches;
                    best = candidateTrail;
                }
            }
        }
        
        if (exact) {
            // If there was an exact match, it would have returned above
            return null;
        } else {
            if (containFullTrail) {
                if (partMatches >= matchTrail.size()) {
                    return best;
                } else {
                    return null;
                }
            } else {
                return best;
            }
        }
    }
    
    
    /**
     * Returns nextLevel from trailed category.
     * <p>
     * Ie for "1/SYRACUS2_CATEGORY/FICTION_C/" the returned value would be 2.
     */
    public static int getNextLevelFromCategoryId(String productCategoryId, DispatchContext dctx) {
        try {
            if (productCategoryId.contains("/")) {
                String[] productCategories = productCategoryId.split("/");
                int level = Integer.parseInt(productCategories[0]);
                return level++;
            } else {
                return 0;
            }
        } catch(Exception e) {
            return 0;
        }
    }
    
    /**
     * Returns proper FacetFilter from trailed category.
     * <p>
     * Ie for "1/SYRACUS2_CATEGORY/FICTION_C/" the returned value would be
     * "2/SYRACUS2_CATEGORY/FICTION_C/".
     */
    public static String getFacetFilterForCategory(String productCategoryId, DispatchContext dctx) {
        try {
            String[] productCategories = productCategoryId.split("/");
            int level = Integer.parseInt(productCategories[0]);
            int nextLevel = level+1;
            productCategories[0] = ""+nextLevel;
            // 2016-03-22: Preserve the original ending / if there was one
            if (productCategoryId.endsWith("/")) {
                return StringUtils.join(productCategories,"/") + "/";
            } else {
                return StringUtils.join(productCategories,"/");
            }
        } catch(Exception e) {
            return productCategoryId;
        }
    }
    
    public static Map<String, Object> categoriesAvailable(String catalogId, String categoryId, String productId, 
            boolean displayproducts, int viewIndex, int viewSize, List<String> queryFilters, Boolean excludeVariants) {
        return categoriesAvailable(catalogId, categoryId, productId, null, displayproducts, viewIndex, viewSize, queryFilters, excludeVariants, null);
    }
    
    public static Map<String, Object> categoriesAvailable(String catalogId, String categoryId, String productId, 
            String facetPrefix, boolean displayproducts, int viewIndex, int viewSize, List<String> queryFilters, Boolean excludeVariants) {
        return categoriesAvailable(catalogId, categoryId, productId, facetPrefix, displayproducts, viewIndex, viewSize, queryFilters, excludeVariants, null);
    }

    public static Map<String, Object> categoriesAvailable(String catalogId, String categoryId, String productId, 
            String facetPrefix, boolean displayproducts, int viewIndex, int viewSize, List<String> queryFilters, Boolean excludeVariants, String core) {
        // create the data model
        Map<String, Object> result = new HashMap<>();
        HttpSolrClient client = null;
        QueryResponse returnMap = new QueryResponse();
        try {
            // do the basic query
            client = SolrUtil.getHttpSolrClient(core);
            // create Query Object
            String query = "inStock[1 TO *]";
            if (categoryId != null)
                query += " +cat:"+ SolrExprUtil.escapeTermFull(categoryId);
            else if (productId != null)
                query += " +productId:" + SolrExprUtil.escapeTermFull(productId);
            SolrQuery solrQuery = new SolrQuery();
            solrQuery.setQuery(query);

            if (catalogId != null)
                solrQuery.addFilterQuery("+catalog:" + SolrExprUtil.escapeTermFull(catalogId));
            
            if (excludeVariants == null) excludeVariants = SolrProductSearch.excludeVariantsDefault;
            if (excludeVariants)
                SolrProductUtil.addExcludeVariantsFilter(solrQuery);
            
            if (displayproducts) {
                if (viewSize > -1) {
                    solrQuery.setRows(viewSize);
                } else
                    solrQuery.setRows(50000);
                if (viewIndex > -1) {
                    // 2016-04-01: This must be calculated
                    //solrQuery.setStart(viewIndex);
                    if (viewSize > 0) {
                        solrQuery.setStart(viewSize * viewIndex);
                    }
                }
            } else {
                solrQuery.setFields("cat");
                solrQuery.setRows(0);
            }
            
            if(UtilValidate.isNotEmpty(facetPrefix)){
                solrQuery.setFacetPrefix(facetPrefix);
            }
            
            solrQuery.setFacetMinCount(0);
            solrQuery.setFacet(true);
            solrQuery.addFacetField("cat");
            solrQuery.setFacetLimit(-1);
            if (Debug.verboseOn()) Debug.logVerbose("solr: solrQuery: " + solrQuery, module);
            returnMap = client.query(solrQuery,METHOD.POST);
            result.put("rows", returnMap);
            result.put("numFound", returnMap.getResults().getNumFound());
        } catch (Exception e) {
            Debug.logError(e.getMessage(), module);
        }
        return result;
    }
}
