package com.ilscipio.scipio.product.category;

import java.util.List;

import org.ofbiz.base.util.GeneralException;

@SuppressWarnings("serial")
public class CategoryTraversalException extends GeneralException {
    public CategoryTraversalException() { super(); }
    public CategoryTraversalException(List<?> messages, Throwable nested) { super(messages, nested); }
    public CategoryTraversalException(List<?> messages) { super(messages); }
    public CategoryTraversalException(String msg, List<?> messages, Throwable nested) { super(msg, messages, nested); }
    public CategoryTraversalException(String msg, List<?> messages) { super(msg, messages); }
    public CategoryTraversalException(String msg, Throwable nested) { super(msg, nested); }
    public CategoryTraversalException(String msg) { super(msg); }
    public CategoryTraversalException(Throwable nested) { super(nested); }
    
    /**
     * CLEAN STOP - Visitor or Traverser may call this to request a clean stop to traversal; not an error; not logged.
     */
    public static class StopCategoryTraversalException extends CategoryTraversalException {
        public StopCategoryTraversalException() { super(); }
        public StopCategoryTraversalException(List<?> messages) { super(messages); }
        public StopCategoryTraversalException(String msg, List<?> messages) { super(msg, messages); }
        public StopCategoryTraversalException(String msg) { super(msg); }
    }
    
}