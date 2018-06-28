package com.ilscipio.scipio.content.content;

import org.ofbiz.base.util.GeneralException;
import org.ofbiz.entity.GenericValue;

/**
 * SCIPIO: Content entity visitor interface.
 * 
 * @see ContentTraverser
 */
public interface ContentEntityVisitor {
    
    /**
     * Visit callback for any entity: Content, DataResource, ...
     */
    void visitEntity(GenericValue entityValue) throws GeneralException;
    
    public static abstract class AbstractContentEntityVisitor implements ContentEntityVisitor {
        @Override public void visitEntity(GenericValue entityValue) throws GeneralException { ; }
    }
}