package com.ilscipio.scipio.product.seo;

import java.util.List;

import org.ofbiz.base.util.GeneralException;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.service.LocalDispatcher;

import com.ilscipio.scipio.product.category.CatalogTraverser;
import com.ilscipio.scipio.product.category.CatalogTraverser.TraversalState;

/**
 * CategoryTraverser that adds UrlGenStats and stateful utils for SEO URL ops.
 */
public abstract class SeoCatalogTraverser extends CatalogTraverser {

    protected UrlGenStats stats = null;

    public SeoCatalogTraverser(Delegator delegator, LocalDispatcher dispatcher, boolean useCache,
            boolean doCategory, boolean doProduct) {
        super(delegator, dispatcher, useCache, doCategory, doProduct, true, null);
    }

    public class SeoTraversalState extends TraversalState {
        public SeoTraversalState(List<GenericValue> trailCategories, int physicalDepth) {
            super(trailCategories, physicalDepth);
        }

        public SeoTraversalState(SeoTraversalState other, boolean deepCopy) {
            super(other, deepCopy);
        }

        @Override
        public SeoTraversalState copy(boolean deepCopy) {
            return new SeoTraversalState(this, deepCopy);
        }
    }
    
    @Override
    protected TraversalState newTraversalState(List<GenericValue> trailCategories, int physicalDepth) {
        return new SeoTraversalState(trailCategories, physicalDepth);
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
