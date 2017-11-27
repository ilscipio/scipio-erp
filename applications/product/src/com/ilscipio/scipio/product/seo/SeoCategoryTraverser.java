package com.ilscipio.scipio.product.seo;

import org.ofbiz.base.util.GeneralException;
import org.ofbiz.entity.Delegator;
import org.ofbiz.service.LocalDispatcher;

import com.ilscipio.scipio.product.category.CategoryTraverser;

/**
 * CategoryTraverser that adds UrlGenStats and stateful utils for SEO URL ops.
 */
public abstract class SeoCategoryTraverser extends CategoryTraverser {

    protected UrlGenStats stats = null;

    public SeoCategoryTraverser(Delegator delegator, LocalDispatcher dispatcher, boolean useCache,
            boolean doCategory, boolean doProduct) {
        super(delegator, dispatcher, useCache, doCategory, doProduct, true, null);
    }

    /**
     * Resets all stateful fields for a new iteration.
     * <p>
     * TODO: REVIEW: currently ambiguous whether better to call this in constructors or at
     * beginning of new operations... both imperfect. maybe let subclasses decide.
     */
    public void reset() throws GeneralException {
        resetStats();
    }
    
    protected void resetStats() {
        this.stats = createStats();
    }

    protected UrlGenStats createStats() {
        return new UrlGenStats(isDoProduct(), isDoCategory(), false);
    }
    
    public UrlGenStats getStats() {
        return stats;
    }
    
}
