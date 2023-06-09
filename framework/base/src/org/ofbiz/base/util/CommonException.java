package org.ofbiz.base.util;

import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * Common exception methods for {@link GeneralException} and {@link GeneralRuntimeException} and all inheritors.
 *
 * <p>SCIPIO: 3.0.0: Added to unify checked and unchecked exception interfaces/handling.</p>
 */
public interface CommonException extends PropertyMessageEx.SettablePropertyMessageEx {

    /**
     * Setter for main property message, which overrides the main detail message returned by {@link Throwable#getMessage()}.
     *
     * <p>NOTE: Use of this method was discouraged - constructors were preferred - because it overrides the detail message in
     * {@link Throwable#getMessage()}, whereas {@link #setPropertyMessageList(Collection)} does not, meaning the constructor detail message implicitly
     * suggests it is more essential than this property message, which is an erroneous suggestion; but it may still be acceptably clear enough
     * if using only a nested exception constructor.</p>
     *
     * <p>Workaround for constructor inheritance. Returns the exception so that can be easily chained in a throw statement.</p>
     *
     * <p>SCIPIO: 2.x.x: Added.</p>
     */
    CommonException setPropertyMessage(PropertyMessage propertyMessage);

    /**
     * Setter for property messages, which may be in the form of any type convertible to PropertyMessage including simple Strings,
     * and which are in addition to the main detail message and typically are not included in {@link Throwable#getMessage()}.
     *
     * <p>Workaround for constructor inheritance. Returns the exception so that can be easily chained in a throw statement.</p>
     *
     * <p>SCIPIO: 2.x.x: Added.</p>
     */
    CommonException setPropertyMessageList(Collection<?> messageList);

    /**
     * Replaces the general-purpose exception properties, similar to *.properties files, for passing information
     * to internal code, debugging and logging; should not be printed to public output.
     *
     * <p>NOTE: Not thread-safe; written for correctness; uses {@link LinkedHashMap} but not enforced by this method as
     * the properties map type is specified entirely by the caller and used as-is; pass null to clear properties.
     * The extending class may also override {@link #makeProperties(Map)} to change the default map type.</p>
     *
     * <p>SCIPIO: 2.1.0: Added.</p>
     */
    CommonException setProperties(Map<String, ?> properties);

    /**
     * Replaces the general-purpose exception properties, similar to *.properties files, for passing information
     * to internal code, debugging and logging; should not be printed to public output.
     *
     * <p>NOTE: Not thread-safe; written for correctness; uses {@link LinkedHashMap}.</p>
     *
     * <p>SCIPIO: 2.1.0: Added.</p>
     */
    CommonException setProperties(Object... properties);

    /**
     * Adds and replaces general-purpose exception properties, similar to *.properties files, for passing information to
     * internal code, debugging and logging; should not be printed to public output.
     *
     * <p>NOTE: Not thread-safe; written for correctness; uses {@link LinkedHashMap}.</p>
     *
     * <p>SCIPIO: 2.1.0: Added.</p>
     */
    CommonException addProperties(Map<String, ?> properties);

    /**
     * Replaces the general-purpose exception properties, similar to *.properties files, for passing information
     * to internal code, debugging and logging; should not be printed to public output.
     *
     * <p>NOTE: Not thread-safe; written for correctness; uses {@link LinkedHashMap}.</p>
     *
     * <p>SCIPIO: 2.1.0: Added.</p>
     */
    CommonException addProperties(Object... properties);

    /**
     * Makes a new exception properties map untied to the exception, for internal and client factory and code use.
     *
     * <p>NOTE: Not thread-safe; written for correctness; uses {@link LinkedHashMap} but extending classes may override
     * to change the default map type.</p>
     *
     * <p>SCIPIO: 2.1.0: Added.</p>
     */
    Map<String, Object> makeProperties(Map<String, ?> sourceProperties);

    /**
     * Clears the general-purpose exception properties, similar to *.properties files, for passing information
     * to internal code, debugging and logging; should not be printed to public output.
     *
     * <p>NOTE: Not thread-safe; written for correctness; uses {@link LinkedHashMap}.</p>
     *
     * <p>SCIPIO: 2.1.0: Added.</p>
     */
    CommonException clearProperties();

    /**
     * Returns a PropertyMessage representation of the exception main detail message, which replaces the detail message
     * returned by {@link Throwable#getMessage()}.
     *
     * <p>Never returns null, even if the main detail message is null; in such case, can return a StaticPropertyMessage that returns null.</p>
     *
     * <p>Unlike {@link Throwable#getMessage()} this may not return the cause error; however, this makes it more appropriate for user-visible errors.</p>
     */
    @Override
    PropertyMessage getPropertyMessage();

    /**
     * Returns a separate additional property message list, separate from the main exception detail message.
     *
     * <p>Can be used to carry localized public-facing messages apart from the internal main detail message (in which
     * case {@link #getPropertyMessage()} is not needed). Pass to {@link PropertyMessage} helper methods to produce localized messages.</p>
     *
     * <p>SCIPIO: 2.0.0: Added earlier.</p>
     *
     * @see #getMessageList(Locale)
     * @see PropertyMessageExUtil#makePropertyMessageList(Collection)
     * @see PropertyMessageExUtil#getExceptionMessageList(Throwable, Locale)
     * @see PropertyMessageExUtil#getCombinedExceptionMessageListOrDetailMessage(Throwable, Locale)
     */
    @Override
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
     * Returns read-only general-purpose exception properties, similar to *.properties files, for passing information
     * to internal code, debugging and logging; should not be printed to public output.
     *
     * <p>NOTE: Not thread-safe; written for correctness; uses {@link LinkedHashMap}.</p>
     */
    Map<String, Object> getProperties();

}
