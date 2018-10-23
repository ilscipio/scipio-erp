package org.ofbiz.base.util;

import java.util.Collection;
import java.util.List;

/**
 * SCIPIO: Interface for exceptions that allow passing around a PropertyMessage representing the
 * main exception message in PropertyMessage form, and additionally a separate list
 * of property messages that behaves like and is meant to be used like a services
 * errorMessageList or successMessageList.
 * <p>
 * NOTE: The message list is meant to be user-friendly like services message lists.
 * If the exception needs to pass a list of more "exception-like" error messages,
 * it should manage a separate list of such messages.
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

    /**
     * Setter methods for property message and lists, for exceptions that choose to
     * support this separate from the constructor.
     * <p>
     * This is needed because changing the exception constructor hierarchies is prohibitively
     * intrusive, despite that modifying exceptions post-constructors is not very common
     * (but is not disallowed).
     */
    public interface SettablePropertyMessageEx extends PropertyMessageEx {
        /**
         * SCIPIO: Setter for property message.
         * Workaround for massive constructor inheritance.
         * Returns the exception so that can be easily chained in a throw statement.
         */
        public SettablePropertyMessageEx setPropertyMessage(PropertyMessage propertyMessage);

        /**
         * SCIPIO: Setter for property messages, which may be in the form of any type
         * convertible to PropertyMessage including simple Strings.
         * Workaround for massive constructor inheritance.
         * Returns the exception so that can be easily chained in a throw statement.
         * <p>
         * DEV NOTE: For implementation, simply pass the messageList to
         * {@link PropertyMessageExUtil#makePropertyMessageList(Collection)} and store result
         * in the instance.
         */
        public SettablePropertyMessageEx setPropertyMessageList(Collection<?> messageList);
    }
}