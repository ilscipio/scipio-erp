package com.ilscipio.scipio.product.seo;

import java.util.Map;

import org.ofbiz.base.util.GeneralException;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.service.LocalDispatcher;

public class SeoCatalogUrlExporter extends SeoCatalogTraverser {

    public static final String module = SeoCatalogUrlExporter.class.getName();
    
    static final String logPrefix = "Seo: Alt URL Export: ";

    public SeoCatalogUrlExporter(Delegator delegator, LocalDispatcher dispatcher, ExportTraversalConfig travConfig) throws GeneralException {
        super(delegator, dispatcher, travConfig);
        this.reset();
    }

    public static class ExportTraversalConfig extends SeoTraversalConfig {
        protected Map<String, ?> servCtxOpts;

        public Map<String, ?> getServCtxOpts() {
            return servCtxOpts;
        }

        public ExportTraversalConfig setServCtxOpts(Map<String, ?> servCtxOpts) {
            this.servCtxOpts = servCtxOpts;
            return this;
        }
    }
    
    @Override
    public ExportTraversalConfig newTravConfig() {
        return new ExportTraversalConfig();
    }

    @Override
    public ExportTraversalConfig getTravConfig() {
        return (ExportTraversalConfig) travConfig;
    }
    
    
    @Override
    public void reset() throws GeneralException {
        super.reset();
    }

    @Override
    protected String getLogMsgPrefix() {
        return logPrefix;
    }

    @Override
    protected String getLogErrorPrefix() {
        return getLogMsgPrefix()+"Error exporting alternative links: ";
    }
    
    @Override
    public void visitCategory(GenericValue productCategory, TraversalState state)
            throws GeneralException {
        //generateCategoryAltUrls(productCategory);
    }

    @Override
    public void visitProduct(GenericValue product, TraversalState state)
            throws GeneralException {
        //generateProductAltUrls(product, true);
    }
    
}
