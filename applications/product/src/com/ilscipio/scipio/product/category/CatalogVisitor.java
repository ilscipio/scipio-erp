package com.ilscipio.scipio.product.category;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang.StringUtils;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.entity.GenericValue;

/**
 * Versatile visitor interface for {@link CatalogTraverser}.
 * @see CatalogTraverser
 */
public interface CatalogVisitor {

    void pushCategory(GenericValue productCategory, List<GenericValue> trailCategories, int physicalDepth) throws GeneralException;
    
    void popCategory(GenericValue productCategory, List<GenericValue> trailCategories, int physicalDepth) throws GeneralException;
    
    void visitCategory(GenericValue productCategory, List<GenericValue> trailCategories, int physicalDepth) throws GeneralException;

    void visitProduct(GenericValue product, List<GenericValue> trailCategories, int physicalDepth) throws GeneralException;

    public static abstract class AbstractCatalogVisitor implements CatalogVisitor {
        @Override public void pushCategory(GenericValue productCategory, List<GenericValue> trailCategories, int physicalDepth) throws GeneralException { ; }
        @Override public void popCategory(GenericValue productCategory, List<GenericValue> trailCategories, int physicalDepth) throws GeneralException { ; }
        @Override public void visitCategory(GenericValue productCategory, List<GenericValue> trailCategories, int physicalDepth) throws GeneralException { ; }
        @Override public void visitProduct(GenericValue product, List<GenericValue> trailCategories, int physicalDepth) throws GeneralException { ; }
    }

    public static class LoggingCatalogVisitor extends AbstractCatalogVisitor {
        public static final String module = LoggingCatalogVisitor.class.getName();
                
        protected List<String> trailIds = new ArrayList<>();
        protected String lastId = null;
        

        @Override
        public void pushCategory(GenericValue productCategory, List<GenericValue> trailCategories, int physicalDepth) {
            trailIds.add(productCategory.getString("productCategoryId"));
        }
        
        @Override
        public void popCategory(GenericValue productCategory, List<GenericValue> trailCategories, int physicalDepth) {
            trailIds.remove(trailIds.size() - 1);
        }
        
        @Override
        public void visitCategory(GenericValue productCategory, List<GenericValue> trailCategories, int physicalDepth) {
            Debug.logInfo(getTrailPrefix() + productCategory.get("productCategoryId") + " [category]", module); 
        }

        @Override
        public void visitProduct(GenericValue product, List<GenericValue> trailCategories, int physicalDepth) {
            Debug.logInfo(getTrailPrefix() + product.get("productId") + " [product]", module); 
        }

        protected String getTrailPrefix() {
            if (trailIds.isEmpty()) return "/";
            else return "/" + StringUtils.join(trailIds, "/") + "/";
        }
    }
}