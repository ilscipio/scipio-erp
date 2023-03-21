/*******************************************************************************
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 *******************************************************************************/
package org.ofbiz.base.util;

import java.io.PrintStream;
import java.io.PrintWriter;
import java.util.*;

import org.ofbiz.base.util.PropertyMessageEx.SettablePropertyMessageEx;

/**
 * Base OFBiz Exception, provides nested exceptions, etc
 * <p>
 * SCIPIO: 2017-11-22: Modified to implement PropertyMessageEx and support a localized main detail
 * message in addition to a separate message list (DEV NOTE: derived from exception enhancements used in cms).
 * This allows localization to be done by callers, at the correct place and time, and where otherwise impossible.
 */
@SuppressWarnings("serial")
public class GeneralException extends Exception implements SettablePropertyMessageEx {

    public static <T> T checkException(Throwable t) throws GeneralException {
        return GeneralException.<T>checkException(t.getMessage(), t);
    }

    public static <T> T checkException(String message, Throwable t) throws GeneralException {
        if (t instanceof Error) {
            throw (Error) t;
        }
        if (t instanceof RuntimeException) {
            throw (RuntimeException) t;
        }
        if (t instanceof GeneralException) {
            throw (GeneralException) t;
        }
        throw (GeneralException) new GeneralException(message).initCause(t);
    }

    /**
     * A PropertyMessage version of the main exception message.
     * <p>SCIPIO: 2.0.0: Added earlier.</p>
     */
    private PropertyMessage propertyMessage;

    /**
     * Additional messages, that work in service-like manner, in ADDITION to the main exception message.
     * The main exception message is NOT counted in this.
     * <p>SCIPIO: Modified to contain PropertyMessage instances instead of Strings.
     * NOTE: we also make this safer by always wrapping in a new ArrayList that caller can't mess up.</p>
     * <p>SCIPIO: 2017-11-21: Added.</p>
     */
    private List<PropertyMessage> propertyMessageList;

    /**
     * General-purposes readable exception properties, similar to *.properties files, for passing information to
     * internal code, debugging and logging; should not be printed to public output.
     * <p>NOTE: Not thread-safe; written for correctness; uses {@link LinkedHashMap} but may be overridden using
     * {@link #setProperties(Map)}.</p>
     * <p>SCIPIO: 2.1.0: Added.</p>
     */
    private Map<String, Object> properties;

    /**
     * Creates new <code>GeneralException</code> without detail message.
     */
    public GeneralException() { }

    /**
     * Constructs an <code>GeneralException</code> with the specified detail message.
     * @param msg the detail message.
     */
    public GeneralException(String msg) {
        super(msg);
    }

    /**
     * Constructs an <code>GeneralException</code> with the specified detail message and nested Exception.
     * @param msg the detail message.
     * @param nested the nested exception.
     */
    public GeneralException(String msg, Throwable nested) {
        super(msg, nested);
    }

    /**
     * Constructs an <code>GeneralException</code> with the specified detail message and nested Exception.
     * @param nested the nested exception.
     */
    public GeneralException(Throwable nested) {
        super(nested);
    }

    /**
     * Constructs an <code>GeneralException</code> with the specified detail message, list and nested Exception.
     * SCIPIO: 2017-11-21: messages can be either pre-localized or hardcoded Strings or PropertyMessage instances.
     * @param msg the detail message.
     * @param messageList error message list.
     */
    public GeneralException(String msg, Collection<?> messageList) {
        super(msg);
        this.propertyMessageList = PropertyMessageExUtil.makePropertyMessageList(messageList); // SCIPIO: make property messages
    }

    /**
     * Constructs an <code>GeneralException</code> with the specified detail message, list and nested Exception.
     * SCIPIO: 2017-11-21: messages can be either pre-localized or hardcoded Strings or PropertyMessage instances.
     * @param msg the detail message.
     * @param messageList error message list.
     * @param nested the nexted exception
     */
    public GeneralException(String msg, Collection<?> messageList, Throwable nested) {
        super(msg, nested);
        this.propertyMessageList = PropertyMessageExUtil.makePropertyMessageList(messageList); // SCIPIO: make property messages
    }

    /**
     * Constructs an <code>GeneralException</code> with the specified detail message list and nested Exception.
     * SCIPIO: 2017-11-21: messages can be either pre-localized or hardcoded Strings or PropertyMessage instances.
     * @param messageList error message list.
     * @param nested the nested exception.
     */
    public GeneralException(Collection<?> messageList, Throwable nested) {
        super(nested);
        this.propertyMessageList = PropertyMessageExUtil.makePropertyMessageList(messageList); // SCIPIO: make property messages
    }

    /**
     * Constructs an <code>GeneralException</code> with the specified detail message list.
     * SCIPIO: 2017-11-21: messages can be either pre-localized or hardcoded Strings or PropertyMessage instances.
     * @param messageList error message list.
     */
    public GeneralException(Collection<?> messageList) {
        this.propertyMessageList = PropertyMessageExUtil.makePropertyMessageList(messageList); // SCIPIO: make property messages
    }

    /**
     * SCIPIO: Constructs an <code>GeneralException</code> with the specified property message,
     * also used as detail message.
     * @param propMsg the property and detail message.
     */
    public GeneralException(PropertyMessage propMsg) {
        super(propMsg.getDefExLocaleMessage());
        this.propertyMessage = propMsg;
    }

    /**
     * SCIPIO: Constructs an <code>GeneralException</code> with the specified property message,
     * also used as detail message, and nested Exception.
     * @param propMsg the property and detail message.
     * @param nested the nested exception.
     */
    public GeneralException(PropertyMessage propMsg, Throwable nested) {
        super(propMsg.getDefExLocaleMessage(), nested);
        this.propertyMessage = propMsg;
    }

    /**
     * SCIPIO: Setter for property message.
     * Workaround for massive constructor inheritance.
     * Returns GeneralException so that can be easily chained in a throw statement.
     */
    @Override
    public GeneralException setPropertyMessage(PropertyMessage propertyMessage) {
        this.propertyMessage = propertyMessage;
        return this;
    }

    /**
     * SCIPIO: Setter for message list including property messages.
     * Workaround for massive constructor inheritance.
     * Returns GeneralException so that can be easily chained in a throw statement.
     */
    @Override
    public GeneralException setPropertyMessageList(Collection<?> messageList) {
        this.propertyMessageList = PropertyMessageExUtil.makePropertyMessageList(messageList); // SCIPIO: make property messages
        return this;
    }

    /**
     * Replaces the general-purpose exception properties, similar to *.properties files, for passing information
     * to internal code, debugging and logging; should not be printed to public output.
     * <p>NOTE: Not thread-safe; written for correctness; uses {@link LinkedHashMap} but not enforced by this method as
     * the properties map type is specified entirely by the caller and used as-is; pass null to clear properties.
     * The extending class may also override {@link #makeProperties(Map)} to change the default map type.</p>
     * <p>SCIPIO: 2.1.0: Added.</p>
     */
    public GeneralException setProperties(Map<String, ?> properties) {
        this.properties = UtilGenerics.cast(properties);
        return this;
    }

    /**
     * Replaces the general-purpose exception properties, similar to *.properties files, for passing information
     * to internal code, debugging and logging; should not be printed to public output.
     * <p>NOTE: Not thread-safe; written for correctness; uses {@link LinkedHashMap}.</p>
     * <p>SCIPIO: 2.1.0: Added.</p>
     */
    public GeneralException setProperties(Object... properties) {
        return setProperties(UtilMisc.orderedMap(properties));
    }

    /**
     * Adds and replaces general-purpose exception properties, similar to *.properties files, for passing information to
     * internal code, debugging and logging; should not be printed to public output.
     * <p>NOTE: Not thread-safe; written for correctness; uses {@link LinkedHashMap}.</p>
     * <p>SCIPIO: 2.1.0: Added.</p>
     */
    public GeneralException addProperties(Map<String, ?> properties) {
        if (UtilValidate.isEmpty(properties)) {
            return this;
        }
        Map<String, Object> currentProperties = this.properties;
        boolean newProperties = false;
        if (currentProperties == null) {
            newProperties = true;
            currentProperties = makeProperties(null);
        }
        currentProperties.putAll(properties);
        if (newProperties) {
            this.properties = currentProperties;
        }
        return this;
    }

    /**
     * Replaces the general-purpose exception properties, similar to *.properties files, for passing information
     * to internal code, debugging and logging; should not be printed to public output.
     * <p>NOTE: Not thread-safe; written for correctness; uses {@link LinkedHashMap}.</p>
     * <p>SCIPIO: 2.1.0: Added.</p>
     */
    public GeneralException addProperties(Object... properties) {
        return addProperties(UtilMisc.orderedMap(properties));
    }

    /**
     * Makes a new exception properties map untied to the exception, for internal and client factory and code use.
     * <p>NOTE: Not thread-safe; written for correctness; uses {@link LinkedHashMap} but extending classes may override
     * to change the default map type.</p>
     * <p>SCIPIO: 2.1.0: Added.</p>
     */
    public Map<String, Object> makeProperties(Map<String, ?> sourceProperties) {
        return UtilValidate.isNotEmpty(sourceProperties) ? new LinkedHashMap<>(sourceProperties) : new LinkedHashMap<>();
    }

    /**
     * Clears the general-purpose exception properties, similar to *.properties files, for passing information
     * to internal code, debugging and logging; should not be printed to public output.
     * <p>NOTE: Not thread-safe; written for correctness; uses {@link LinkedHashMap}.</p>
     * <p>SCIPIO: 2.1.0: Added.</p>
     */
    public GeneralException clearProperties() {
        this.properties = null;
        return this;
    }

    /**
     * Returns read-only general-purpose exception properties, similar to *.properties files, for passing information
     * to internal code, debugging and logging; should not be printed to public output.
     * <p>NOTE: Not thread-safe; written for correctness; uses {@link LinkedHashMap}.</p>
     * <p>SCIPIO: 2.1.0: Added.</p>
     */
    public Map<String, Object> getProperties() {
        Map<String, Object> properties = this.properties;
        return UtilValidate.isNotEmpty(properties) ? Collections.unmodifiableMap(properties) : Collections.emptyMap();
    }

    /**
     * Returns the detail message, including the message from the nested exception if there is one.
     * SCIPIO: modified to include the property message instead of the detail message IF it is set.
     */
    @Override
    public String getMessage() {
        Throwable nested = getCause();
        if (nested != null) {
            // SCIPIO
            //if (super.getMessage() == null) {
            //    return nested.getMessage();
            //} else {
            //    return super.getMessage() + " (" + nested.getMessage() + ")";
            //}
            String detailMessage = getDefExLocalePropertyOrDetailMessage();
            if (detailMessage == null) {
                return nested.getMessage();
            } else {
                String nestedMessage = nested.getMessage();
                if (nestedMessage == null) {
                    return detailMessage;
                } else {
                    return detailMessage + " (" + nestedMessage + ")";
                }
            }
        } else {
            // SCIPIO
            //return super.getMessage();
            return getDefExLocalePropertyOrDetailMessage();
        }
    }

    /**
     * Returns the list of messages attached to this exception.
     * <p>SCIPIO: Use of this is now discouraged (even if caller pre-localized);
     * use {@link #getPropertyMessageList()} or {@link #getMessageList(Locale)} instead.
     * This returns only non-localized messages, unless the exception sender
     * pre-localized them. It uses the default/fallback property locale ({@link UtilProperties#getFallbackLocale()}),
     * because this fits the majority use cases.</p>
     * <p>SCIPIO: 2.0.0: Modified earlier to produce message list using propertyMessageList and default locale.</p>
     * @see #getMessageList(Locale)
     * @see #getPropertyMessageList()
     */
    public List<String> getMessageList() {
        return PropertyMessage.getDefPropLocaleMessages(this.propertyMessageList); // SCIPIO: property messages
    }

    /**
     * Returns the list of messages attached to this exception, localized if possible.
     * <p>NOTE: Depending on how the messages were added to the exception, these are not guaranteed
     * to be localized or may even be in a different language.</p>
     * <p>SCIPIO: 2.0.0: Modified earlier to produce message list using propertyMessageList and specified locale.</p>
     * @see #getPropertyMessageList()
     */
    public List<String> getMessageList(Locale locale) {
        return PropertyMessage.getMessages(this.propertyMessageList, locale); // SCIPIO: property messages
    }

    /**
     * Returns the list of attached messages as PropertyMessage instances; may be null.
     * <p>Can be passed to {@link PropertyMessage} helper methods to produce localized messages.</p>
     * <p>SCIPIO: 2.0.0: Added earlier.</p>
     * @see #getMessageList(Locale)
     */
    @Override
    public List<PropertyMessage> getPropertyMessageList() {
        return propertyMessageList;
    }

    /** Returns the detail message, NOT including the message from the nested exception. */
    public String getNonNestedMessage() {
        return super.getMessage();
    }

    /** Returns the nested exception if there is one, null if there is not. */
    public Throwable getNested() {
        Throwable nested = getCause();
        if (nested == null) {
            return this;
        }
        return nested;
    }

    /** Prints the composite message to System.err. */
    @Override
    public void printStackTrace() {
        super.printStackTrace();
    }

    /** Prints the composite message and the embedded stack trace to the specified stream ps. */
    @Override
    public void printStackTrace(PrintStream ps) {
        super.printStackTrace(ps);
    }

    /** Prints the composite message and the embedded stack trace to the specified print writer pw. */
    @Override
    public void printStackTrace(PrintWriter pw) {
        super.printStackTrace(pw);
    }

    /**
     * SCIPIO: Returns a PropertyMessage representation of the main exception message.
     * Never returns null - if exception message was null, it will
     * return a StaticPropertyMessage that wraps a null message.
     * NOTE: unlike {@link #getMessage()} this may not return the cause error; however,
     * this makes it more appropriate for user-visible errors.
     */
    @Override
    public PropertyMessage getPropertyMessage() {
        if (propertyMessage != null) return propertyMessage;
        else return PropertyMessage.makeFromStatic(getMessage());
    }

    /**
     * SCIPIO: If propertyMessage is set, returns it formatted in the default exception locale;
     * if not set, returns the main exception detail message instead (which may be null).
     */
    protected String getDefExLocalePropertyOrDetailMessage() {
        if (propertyMessage != null) return propertyMessage.getDefExLocaleMessage();
        else return super.getMessage();
    }
}

