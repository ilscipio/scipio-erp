package com.ilscipio.scipio.product.category;

import java.util.List;

import org.ofbiz.base.util.GeneralException;

@SuppressWarnings("serial")
public class CatalogTraversalException extends GeneralException {
    public CatalogTraversalException() { super(); }
    public CatalogTraversalException(List<?> messages, Throwable nested) { super(messages, nested); }
    public CatalogTraversalException(List<?> messages) { super(messages); }
    public CatalogTraversalException(String msg, List<?> messages, Throwable nested) { super(msg, messages, nested); }
    public CatalogTraversalException(String msg, List<?> messages) { super(msg, messages); }
    public CatalogTraversalException(String msg, Throwable nested) { super(msg, nested); }
    public CatalogTraversalException(String msg) { super(msg); }
    public CatalogTraversalException(Throwable nested) { super(nested); }

    /**
     * CLEAN STOP - Visitor or Traverser may call this to request a clean stop to traversal; not an error; not logged.
     */
    public static class StopCatalogTraversalException extends CatalogTraversalException {
        public StopCatalogTraversalException() { super(); }
        public StopCatalogTraversalException(List<?> messages) { super(messages); }
        public StopCatalogTraversalException(String msg, List<?> messages) { super(msg, messages); }
        public StopCatalogTraversalException(String msg) { super(msg); }
    }

}