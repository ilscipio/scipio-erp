package com.ilscipio.scipio.solr;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.apache.solr.client.solrj.SolrQuery;
import org.apache.solr.common.SolrDocumentList;
import org.apache.solr.common.SolrException;

/**
 * High-level util to help dealing with SolrQuery object and high-level query ops.
 * For low-level query expression parsing and string/term/phrase manipulation, see {@link SolrExprUtil}.
 * @see SolrExprUtil
 */
public abstract class SolrQueryUtil {

    public static final String module = SolrQueryUtil.class.getName();
    
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
}
