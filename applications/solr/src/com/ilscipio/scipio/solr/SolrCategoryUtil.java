package com.ilscipio.scipio.solr;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import com.ilscipio.scipio.product.product.ProductDataReader;
import org.apache.commons.lang3.StringUtils;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.DelegatorFactory;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.product.catalog.CatalogWorker;
import org.ofbiz.product.category.CategoryWorker;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceDispatcher;

/**
 * Product category util class for solr.
 * TODO: refactor: some of these methods belong in non-solr classes.
 */
public abstract class SolrCategoryUtil {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    /**
     * Gets catalog IDs for specified product category.
     * <p>
     * This method is a supplement to CatalogWorker methods.
     */
    static List<String> getCatalogIdsByCategoryId(Delegator delegator, String productCategoryId, Timestamp moment, boolean useCache) {
        return UtilMisc.getMapValuesForKeyOrNewList(getProdCatalogCategoryByCategoryId(delegator, productCategoryId, moment, useCache), "prodCatalogId");
    }

    static List<String> getCatalogIdsByCategoryId(Delegator delegator, String productCategoryId, Timestamp moment) {
        return getCatalogIdsByCategoryId(delegator, productCategoryId, moment, false); // legacy
    }

    static List<GenericValue> getProdCatalogCategoryByCategoryId(Delegator delegator, String productCategoryId, Timestamp moment, boolean useCache) {
        List<GenericValue> catalogs;
        try {
            catalogs = EntityQuery.use(delegator).from("ProdCatalogCategory").where("productCategoryId", productCategoryId)
                    .filterByDate(moment).orderBy("sequenceNum").cache(useCache).queryList();
        } catch (GenericEntityException e) {
            Debug.logError(e, "Solr: Error looking up catalogs for productCategoryId: " + productCategoryId, module);
            catalogs = new ArrayList<>();
        }
        return catalogs;
    }

    static Map<String, List<String>> getCatalogIdsByCategoryIdMap(Delegator delegator, List<String> categoryIds, Timestamp moment, boolean useCache) {
        Map<String, List<String>> map = new HashMap<>();
        if (categoryIds == null) return map;
        for(String categoryId : categoryIds) {
            map.put(categoryId, UtilMisc.getMapValuesForKeyOrNewList(getProdCatalogCategoryByCategoryId(delegator, categoryId, moment, useCache), "prodCatalogId"));
        }
        return map;
    }

    @Deprecated
    static List<String> getStringFieldList(List<GenericValue> values, String fieldName) {
        return UtilMisc.getMapValuesForKeyOrNewList(values, fieldName);
    }

    @Deprecated
    static void addAllStringFieldList(Collection<String> out, List<GenericValue> values, String fieldName) {
        UtilMisc.getMapValuesForKey(values, fieldName, out);
    }

    public static List<List<String>> getCategoryTrail(String productCategoryId, DispatchContext dctx) {
        return CategoryWorker.getCategoryRollupTrails(dctx.getDelegator(), productCategoryId, true);
    }

    public static List<List<String>> getCategoryTrail(String productCategoryId, DispatchContext dctx, Timestamp moment, boolean ordered, boolean useCache) {
        return CategoryWorker.getCategoryRollupTrails(dctx.getDelegator(), productCategoryId, moment, ordered, useCache);
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
        if (productCategoryId == null) return null;
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
     * with currentTrail instead {@link #getCategoryNameWithTrail(String, String, Boolean, DispatchContext, List)}.
     */
    @Deprecated
    public static String getCategoryNameWithTrail(String productCategoryId, DispatchContext dctx) {
        return getCategoryNameWithTrail(productCategoryId, null, true,  dctx, null);
    }

    /**
     * Returns categoryName with trail.
     * @deprecated You should usually call the overload
     * with currentTrail instead {@link #getCategoryNameWithTrail(String, String, Boolean, DispatchContext, List)}.
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

    public static <C extends Collection<String>> C getCategoryTrails(C trails, DispatchContext dctx, ProductDataReader reader, Collection<String> productCategoryIds, Timestamp moment, boolean ordered, boolean useCache) throws GeneralException {
        for (String productCategoryId : productCategoryIds) {
            formatCategoryTrails(trails, dctx, reader.getCategoryRollupTrails(dctx, productCategoryId, moment, ordered, useCache));
        }
        return trails;
    }

    public static <C extends Collection<String>> C getCategoryTrails(C trails, DispatchContext dctx, Collection<String> productCategoryIds, Timestamp moment, boolean ordered, boolean useCache) throws GeneralException {
        return getCategoryTrails(trails, dctx, ProductDataReader.DEFAULT, productCategoryIds, moment, ordered, useCache);
    }

    public static <C extends Collection<String>> C formatCategoryTrails(C trails, DispatchContext dctx, List<List<String>> trailElements) {
        for (List<String> trail : trailElements) {
            formatCategoryTrail(trails, dctx, trail);
        }
        return trails;
    }

    public static <C extends Collection<String>> C formatCategoryTrail(C trails, DispatchContext dctx, List<String> trail) {
        StringBuilder catMember = new StringBuilder();
        int i = 0;
        for(String trailString : trail) {
            if (catMember.length() > 0){
                catMember.append("/");
                i++;
            }
            catMember.append(trailString);
            String cm = i +"/"+ catMember.toString();
            trails.add(cm);
        }
        return trails;
    }

    public static <C extends Collection<String>> C getCatalogIdsFromCategoryTrails(C catalogIds, DispatchContext dctx, ProductDataReader productDataReader, Collection<String> trails, Timestamp moment, boolean useCache) throws GeneralException {
        Map<String, List<String>> categoryIdCatalogIdMap = new HashMap<>(); // 2017-09: local cache; multiple lookups for same
        for (String trail : trails) {
            String productCategoryId = (trail.split("/").length > 0) ? trail.split("/")[1] : trail;
            List<String> catalogMembers = categoryIdCatalogIdMap.get(productCategoryId);
            if (catalogMembers == null) {
                catalogMembers = productDataReader.getCatalogIdsByCategoryId(dctx, productCategoryId, moment, useCache);
                categoryIdCatalogIdMap.put(productCategoryId, catalogMembers);
            }
            for (String catalogMember : catalogMembers) {
                catalogIds.add(catalogMember);
            }
        }
        return catalogIds;
    }

    public static <C extends Collection<String>> C getCatalogIdsFromCategoryTrails(C catalogIds, DispatchContext dctx, Collection<String> trails, Timestamp moment, boolean useCache) throws GeneralException {
        return getCatalogIdsFromCategoryTrails(catalogIds, dctx, ProductDataReader.DEFAULT, trails, moment, useCache);
    }

    /**
     * @deprecated 2018-05-25: use the solrAvailableCategories or solrSideDeepCategory services instead.
     */
    @Deprecated
    public static Map<String, Object> categoriesAvailable(String catalogId, String categoryId, String productId,
            boolean displayproducts, int viewIndex, int viewSize, List<String> queryFilters, Boolean excludeVariants) {
        return categoriesAvailable(catalogId, categoryId, productId, null, displayproducts, viewIndex, viewSize, queryFilters, excludeVariants, null);
    }

    /**
     * @deprecated 2018-05-25: use the solrAvailableCategories or solrSideDeepCategory services instead.
     */
    @Deprecated
    public static Map<String, Object> categoriesAvailable(String catalogId, String categoryId, String productId,
            String facetPrefix, boolean displayproducts, int viewIndex, int viewSize, List<String> queryFilters, Boolean excludeVariants) {
        return categoriesAvailable(catalogId, categoryId, productId, facetPrefix, displayproducts, viewIndex, viewSize, queryFilters, excludeVariants, null);
    }

    /**
     * @deprecated 2018-05-25: use the solrAvailableCategories or solrSideDeepCategory services instead.
     */
    @Deprecated
    public static Map<String, Object> categoriesAvailable(String catalogId, String categoryId, String productId,
            String facetPrefix, boolean displayproducts, int viewIndex, int viewSize, List<String> queryFilters, Boolean excludeVariants, String core) {
        LocalDispatcher dispatcher = ServiceDispatcher.getLocalDispatcher("default", DelegatorFactory.getDelegator("default"));
        Map<String, Object> context = new HashMap<>();
        context.put("core", core);
        context.put("queryFilters", queryFilters);
        context.put("excludeVariants", excludeVariants);
        return SolrProductSearch.getAvailableCategories(dispatcher.getDispatchContext(), context, catalogId, categoryId, productId, facetPrefix, displayproducts, viewIndex, viewSize);
    }
}
