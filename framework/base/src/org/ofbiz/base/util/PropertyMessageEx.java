package org.ofbiz.base.util;

import java.util.List;

/**
 * Interface for exceptions that allow passing around a PropertyMessage representing the
 * main exception message in PropertyMessage form, and additionally a separate list
 * of property messages that behaves like
 */
public interface PropertyMessageEx {
    
    /**
     * Returns a PropertyMessage representation of the exception main detail message.
     * To simplify client code, THIS MUST NEVER RETURN NULL, even if the main detail message
     * is null. In such case, it can return a StaticPropertyMessage that returns null.
     */
    PropertyMessage getPropertyMessage();
    
    /**
     * Returns a separate additional property message list, separate from the main exception detail message.
     */
    List<PropertyMessage> getPropertyMessageList();
    
}