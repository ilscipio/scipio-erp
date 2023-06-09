package org.ofbiz.base.util;

import java.util.Collection;
import java.util.List;
import java.util.Locale;

/**
 * Interface for exceptions that allow passing around a PropertyMessage representing the
 * main exception message in PropertyMessage form, and additionally a separate list
 * of property messages that behaves like and is meant to be used like a services
 * errorMessageList or successMessageList.
 *
 * <p>NOTE: The message list is meant to be user-friendly like services message lists.
 * If the exception needs to pass a list of more "exception-like" error messages,
 * it should manage a separate list of such messages.</p>
 *
 * <p>SCIPIO: 2.x.x: Added.</p>
 */
public interface PropertyMessageEx {

    /**
     * Returns a PropertyMessage representation of the exception main detail message, which replaces the detail message
     * returned by {@link Throwable#getMessage()}.
     *
     * <p>Never returns null, even if the main detail message is null; in such case, can return a StaticPropertyMessage that returns null.</p>
     */
    PropertyMessage getPropertyMessage();

    /**
     * Returns a separate additional property message list, separate from the main exception detail message.
     *
     * <p>Can be used to carry localized public-facing messages apart from the internal main detail message (in which
     * case {@link #getPropertyMessage()} is not needed). Pass to {@link PropertyMessage} helper methods to produce localized messages.</p>
     *
     * @see #getMessageList(Locale)
     * @see PropertyMessageExUtil#makePropertyMessageList(Collection)
     * @see PropertyMessageExUtil#getExceptionMessageList(Throwable, Locale)
     * @see PropertyMessageExUtil#getCombinedExceptionMessageListOrDetailMessage(Throwable, Locale)
     */
    List<PropertyMessage> getPropertyMessageList();

    /**
     * Returns the list of additional messages attached to this exception, localized if possible.
     *
     * <p>NOTE: Depending on how the messages were added to the exception, these are not guaranteed
     * to be localized or may even be in a different language.</p>
     */
    default List<String> getMessageList(Locale locale) {
        return PropertyMessage.getMessages(getPropertyMessageList(), locale);
    }

    /**
     * Setter methods for property message and lists, for exceptions that choose to
     * support this separate from the constructor.
     *
     * <p>This is needed because changing the exception constructor hierarchies is prohibitively
     * intrusive, despite that modifying exceptions post-constructors is not very common
     * (but is not disallowed).</p>
     */
    interface SettablePropertyMessageEx extends PropertyMessageEx {

        /**
         * Setter for main property message, which overrides the main detail message returned by {@link Throwable#getMessage()}.
         *
         * <p>NOTE: Use of this method was discouraged - constructors were preferred - because it overrides the detail message in
         * {@link Throwable#getMessage()}, whereas {@link #setPropertyMessageList(Collection)} does not, meaning the constructor detail message implicitly
         * suggests it is more essential than this property message, which is an erroneous suggestion; but it may still be acceptably clear enough
         * if using only a nested exception constructor.</p>
         *
         * <p>Workaround for constructor inheritance. Returns the exception so that can be easily chained in a throw statement.</p>
         */
        SettablePropertyMessageEx setPropertyMessage(PropertyMessage propertyMessage);

        /**
         * Setter for property messages, which may be in the form of any type convertible to PropertyMessage including simple Strings,
         * and which are in addition to the main detail message and typically are not included in {@link Throwable#getMessage()}.
         *
         * <p>Workaround for constructor inheritance. Returns the exception so that can be easily chained in a throw statement.</p>
         *
         * @see PropertyMessageExUtil#makePropertyMessageList(Collection)
         */
        SettablePropertyMessageEx setPropertyMessageList(Collection<?> messageList);

    }

}