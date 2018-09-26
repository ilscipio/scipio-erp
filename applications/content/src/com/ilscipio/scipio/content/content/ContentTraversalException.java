package com.ilscipio.scipio.content.content;

import java.util.List;

import org.ofbiz.base.util.GeneralException;

@SuppressWarnings("serial")
public class ContentTraversalException extends GeneralException {
    public ContentTraversalException() { super(); }
    public ContentTraversalException(List<?> messages, Throwable nested) { super(messages, nested); }
    public ContentTraversalException(List<?> messages) { super(messages); }
    public ContentTraversalException(String msg, List<?> messages, Throwable nested) { super(msg, messages, nested); }
    public ContentTraversalException(String msg, List<?> messages) { super(msg, messages); }
    public ContentTraversalException(String msg, Throwable nested) { super(msg, nested); }
    public ContentTraversalException(String msg) { super(msg); }
    public ContentTraversalException(Throwable nested) { super(nested); }

    /**
     * CLEAN STOP - Visitor or Traverser may call this to request a clean stop to traversal; not an error; not logged.
     */
    public static class StopContentTraversalException extends ContentTraversalException {
        public StopContentTraversalException() { super(); }
        public StopContentTraversalException(List<?> messages) { super(messages); }
        public StopContentTraversalException(String msg, List<?> messages) { super(msg, messages); }
        public StopContentTraversalException(String msg) { super(msg); }
    }
}