package com.ilscipio.scipio.solr;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;

import org.apache.solr.client.solrj.SolrQuery;
import org.apache.solr.common.SolrDocumentList;
import org.apache.solr.common.SolrException;
import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.entity.GenericValue;

/**
 * High-level util to help dealing with SolrQuery object and high-level query ops.
 * For low-level query expression parsing and string/term/phrase manipulation, see {@link SolrExprUtil}.
 * @see SolrExprUtil
 */
public abstract class SolrQueryUtil {

    //private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    protected SolrQueryUtil() {
    }

    public static void addFilterQueries(SolrQuery solrQuery, Collection<String> queryFilters) {
        if (queryFilters != null) {
            for(String filter : queryFilters) {
                solrQuery.addFilterQuery(filter);
            }
        }
    }
    
    public static List<String> copyQueryFilters(List<String> queryFilters) {
        return queryFilters != null ? new ArrayList<String>(queryFilters) : new ArrayList<String>();
    }

    /**
     * Returns the closest whole viewIndex.
     */
    public static Integer calcResultViewIndex(SolrDocumentList results, Integer viewSize) {
        Integer viewIndex = null;
        if (results != null && viewSize != null && viewSize > 0) {
            long start = results.getStart();
            viewIndex = (int) (start / (long) viewSize);
        }
        return viewIndex;
    }

    /**
     * Checks if the exception extracted by {@link #getSolrNestedException} is a syntax error.
     * FIXME: AWFUL HEURISTIC
     */
    public static boolean isSolrQuerySyntaxError(Throwable t) {
        // exception message usually contains the string: "org.apache.solr.search.SyntaxError"
        // hopefully this is accurate enough... how else to check? cause is not set and
        // the root SyntaxError is from an inaccessible jar (CANNOT add it to classpath)
        return ((t instanceof SolrException) && t.getMessage().toLowerCase().contains("syntax")); 
    }
    
    /**
     * Adds the default/common query filters for products, including filters for
     * Product inventory and Product.salesDiscontinuationDate, with options corresponding
     * to the attributes of the solrDefaultQueryFilters service interface.
     * <p>
     * NOTE: The defaults for ProductStore.showOutOfStockProducts and ProductStore.showDiscontinuedProducts
     * is FALSE (inherited from stock ofbiz; see entitymodel.xml descriptions for ProductStore).
     * The default for excludeVariants is TRUE.
     */
    public static void addDefaultQueryFilters(List<String> queryFilters, Map<String, ?> context) {
        if (!Boolean.FALSE.equals(context.get("useDefaultFilters"))) {
            SolrProductUtil.addDefaultProductFilters(queryFilters, context);
        }
    }

    /**
     * Adds the default/common query filters for products.
     * @see #addDefaultQueryFilters(List, Map)
     */
    public static void addDefaultQueryFilters(SolrQuery solrQuery, Map<String, ?> context) {
        List<String> queryFilters = new ArrayList<>();
        addDefaultQueryFilters(queryFilters, context);
        for(String filter : queryFilters) {
            solrQuery.addFilterQuery(filter);
        }
    }
}
