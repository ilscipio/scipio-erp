package com.ilscipio.scipio.product.seo;

import java.util.List;

import org.ofbiz.base.util.GeneralException;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.service.LocalDispatcher;

import com.ilscipio.scipio.product.category.CatalogTraverser;

/**
 * CategoryTraverser that adds UrlGenStats and stateful utils for SEO URL ops.
 */
public abstract class SeoCatalogTraverser extends CatalogTraverser {

    protected UrlGenStats stats = null;

    public SeoCatalogTraverser(Delegator delegator, LocalDispatcher dispatcher, SeoTraversalConfig travConfig) {
        super(delegator, dispatcher, travConfig);
    }

    public static class SeoTraversalConfig extends TraversalConfig {
        private boolean doContent = false;

        public boolean isDoContent() {
            return doContent;
        }

        public SeoTraversalConfig setDoContent(boolean doContent) {
            this.doContent = doContent;
            return this;
        }
    }
    
    @Override
    public SeoTraversalConfig newTravConfig() {
        return new SeoTraversalConfig();
    }

    @Override
    public SeoTraversalConfig getTravConfig() {
        return (SeoTraversalConfig) travConfig;
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

    public void reset() throws GeneralException {
        super.reset();
        resetStats();
    }
    
    protected void resetStats() {
        this.stats = createStats();
    }

    protected UrlGenStats createStats() {
        return new UrlGenStats(getTravConfig().isDoProduct(), getTravConfig().isDoCategory(), getTravConfig().isDoContent());
    }
    
    public UrlGenStats getStats() {
        return stats;
    }
    
    // TODO: REVIEW: this is unreliable, does not account for products with child products,
    // so just forget these stats for the time being.
//    // specialized override needed to update the stats
//    protected boolean checkUpdateCategoryDuplicates(String productCategoryId) {
//        if (this.seenCategoryIds.contains(productCategoryId)) {
//            stats.categoryDupSkip++;
//            return true;
//        } else {
//            this.seenCategoryIds.add(productCategoryId);
//            return false;
//        }
//    }
//    
//    // specialized override needed to update the stats
//    protected boolean checkUpdateProductDuplicates(String productId) {
//        if (this.seenProductIds.contains(productId)) {
//            stats.productDupSkip++;
//            return true;
//        } else {
//            this.seenProductIds.add(productId);
//            return false;
//        }
//    }
}
