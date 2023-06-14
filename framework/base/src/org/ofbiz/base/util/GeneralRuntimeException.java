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
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * Base Scipio Runtime Exception, provides nested exceptions, etc
 *
 * <p>This supports a main property message ({@link PropertyMessageEx#getPropertyMessage()}) or traditional superclass
 * detail message (the main property message overrides the superclass detail message in {@link Throwable#getMessage()}),
 * plus an additional list of property messages ({@link PropertyMessageEx#getPropertyMessageList()}). The additional
 * property message list is not included in {@link Throwable#getMessage()} and may be used either in a service-like
 * manner (as is errorMessageList) or as a public-facing localized messages in addition to the internal (english/log)
 * main detail message, depending on point of use. In addition, arbitrary properties can be passed using
 * {@link GeneralRuntimeException#setProperties}.</p>
 *
 * <p>SCIPIO: 3.x.x: Modified to implement PropertyMessageEx and support a localized main detail
 * message in addition to a separate message list (DEV NOTE: derived from exception enhancements used in cms).
 * This allows localization to be done by callers, at the correct place and time, and where otherwise impossible.</p>
 */
@SuppressWarnings("serial")
public class GeneralRuntimeException extends RuntimeException implements CommonException {

    /**
     * A PropertyMessage version of the main exception message, which overrides the detail message in {@link #getMessage()}.
     *
     * <p>NOTE: This overrides the superclass message in {@link #getMessage()}.</p>
     *
     * <p>SCIPIO: 2.0.0: Added earlier.</p>
     */
    private PropertyMessage propertyMessage;

    /**
     * Additional messages, that work in service-like manner, in addition to the main exception message, that do not override
     * the main property or detail messsage in {@link #getMessage()}.
     *
     * <p>NOTE: The main exception message is NOT counted in this, and these are NOT returned by {@link #getMessage()}.
     * This allows to separate an internal (log/english) representation from localized public messages.</p>
     *
     * <p>Always contains PropertyMessage instances (StaticPropertyMessage as needed) instead of Strings.
     * NOTE: we also make this safer by always wrapping in a new ArrayList that caller can't mess up.</p>
     *
     * <p>SCIPIO: 2.x.x: Added.</p>
     *
     * @see PropertyMessageExUtil#getCombinedExceptionMessageListOrDetailMessage(Throwable, Locale)
     */
    private List<PropertyMessage> propertyMessageList;

    /**
     * General-purposes readable exception properties, similar to *.properties files, for passing information to
     * internal code, debugging and logging; should not be printed to public output.
     *
     * <p>NOTE: Not thread-safe; written for correctness; uses {@link LinkedHashMap} but may be overridden using
     * {@link #setProperties(Map)}.</p>
     *
     * <p>SCIPIO: 2.1.0: Added.</p>
     */
    private Map<String, Object> properties;

    /**
     * Creates new <code>GeneralRuntimeException</code> without detail message.
     */
    public GeneralRuntimeException() { }

    /**
     * Constructs an <code>GeneralRuntimeException</code> with the specified detail message.
     *
     * @param msg the detail message.
     */
    public GeneralRuntimeException(String msg) {
        super(msg);
    }

    /**
     * Constructs an <code>GeneralRuntimeException</code> with the specified detail message and nested Exception.
     *
     * @param msg the detail message.
     * @param nested the nested exception.
     */
    public GeneralRuntimeException(String msg, Throwable nested) {
        super(msg, nested);
    }

    /**
     * Constructs an <code>GeneralRuntimeException</code> with the specified detail message and nested Exception.
     *
     * @param nested the nested exception.
     */
    public GeneralRuntimeException(Throwable nested) {
        super(null, nested);
        // Let our getMessage() handle it
        //super(nested);
    }

    /**
     * Constructs an <code>GeneralRuntimeException</code> with the specified detail message, additional message list and nested Exception.
     *
     * <p>SCIPIO: 2.x.x: Messages can be either pre-localized or hardcoded Strings or PropertyMessage instances.</p>
     *
     * @param msg the detail message.
     * @param messageList error message list.
     */
    public GeneralRuntimeException(String msg, Collection<?> messageList) {
        super(msg);
        this.propertyMessageList = PropertyMessageExUtil.makePropertyMessageList(messageList); // SCIPIO: make property messages
    }

    /**
     * Constructs an <code>GeneralRuntimeException</code> with the specified detail message, additional message list and nested Exception.
     *
     * <p>SCIPIO: 2.x.x: Messages can be either pre-localized or hardcoded Strings or PropertyMessage instances.</p>
     *
     * @param msg the detail message.
     * @param messageList error message list.
     * @param nested the nexted exception
     */
    public GeneralRuntimeException(String msg, Collection<?> messageList, Throwable nested) {
        super(msg, nested);
        this.propertyMessageList = PropertyMessageExUtil.makePropertyMessageList(messageList); // SCIPIO: make property messages
    }

    /**
     * Constructs an <code>GeneralRuntimeException</code> with the specified additional message list and nested Exception.
     *
     * <p>SCIPIO: 2.x.x: Messages can be either pre-localized or hardcoded Strings or PropertyMessage instances.</p>
     *
     * @param messageList error message list.
     * @param nested the nested exception.
     */
    public GeneralRuntimeException(Collection<?> messageList, Throwable nested) {
        super(null, nested);
        // Let our getMessage() handle it
        //super(nested);
        this.propertyMessageList = PropertyMessageExUtil.makePropertyMessageList(messageList); // SCIPIO: make property messages
    }

    /**
     * Constructs an <code>GeneralRuntimeException</code> with the specified additional message list.
     *
     * <p>SCIPIO: 2.x.x: Messages can be either pre-localized or hardcoded Strings or PropertyMessage instances.</p>
     *
     * @param messageList error message list.
     */
    public GeneralRuntimeException(Collection<?> messageList) {
        this.propertyMessageList = PropertyMessageExUtil.makePropertyMessageList(messageList); // SCIPIO: make property messages
    }

    /**
     * Constructs an <code>GeneralRuntimeException</code> with the specified property message, also used as detail message.
     *
     * <p>SCIPIO: 2.x.x: Added.</p>
     *
     * @param propMsg the property and detail message.
     */
    public GeneralRuntimeException(PropertyMessage propMsg) {
        // Let our getMessage() handle it
        //super(propMsg != null ? propMsg.getDefExLocaleMessage() : null);
        this.propertyMessage = propMsg;
    }

    /**
     * Constructs an <code>GeneralRuntimeException</code> with the specified property message, also used as detail message, and nested Exception.
     *
     * <p>SCIPIO: 2.x.x: Added.</p>
     *
     * @param propMsg the property and detail message.
     * @param nested the nested exception.
     */
    public GeneralRuntimeException(PropertyMessage propMsg, Throwable nested) {
        super(null, nested);
        // Let our getMessage() handle it
        //super(propMsg != null ? propMsg.getDefExLocaleMessage() : null, nested);
        this.propertyMessage = propMsg;
    }

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
    @Override
    public GeneralRuntimeException setPropertyMessage(PropertyMessage propertyMessage) {
        this.propertyMessage = propertyMessage;
        return this;
    }

    /**
     * Setter for property messages, which may be in the form of any type convertible to PropertyMessage including simple Strings,
     * and which are in addition to the main detail message and typically are not included in {@link Throwable#getMessage()}.
     *
     * <p>Workaround for constructor inheritance. Returns the exception so that can be easily chained in a throw statement.</p>
     *
     * <p>SCIPIO: 2.x.x: Added.</p>
     */
    @Override
    public GeneralRuntimeException setPropertyMessageList(Collection<?> messageList) {
        this.propertyMessageList = PropertyMessageExUtil.makePropertyMessageList(messageList); // SCIPIO: make property messages
        return this;
    }

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
    @Override
    public GeneralRuntimeException setProperties(Map<String, ?> properties) {
        this.properties = UtilGenerics.cast(properties);
        return this;
    }

    /**
     * Replaces the general-purpose exception properties, similar to *.properties files, for passing information
     * to internal code, debugging and logging; should not be printed to public output.
     *
     * <p>NOTE: Not thread-safe; written for correctness; uses {@link LinkedHashMap}.</p>
     *
     * <p>SCIPIO: 2.1.0: Added.</p>
     */
    @Override
    public GeneralRuntimeException setProperties(Object... properties) {
        return setProperties(UtilMisc.orderedMap(properties));
    }

    /**
     * Adds and replaces general-purpose exception properties, similar to *.properties files, for passing information to
     * internal code, debugging and logging; should not be printed to public output.
     *
     * <p>NOTE: Not thread-safe; written for correctness; uses {@link LinkedHashMap}.</p>
     *
     * <p>SCIPIO: 2.1.0: Added.</p>
     */
    @Override
    public GeneralRuntimeException addProperties(Map<String, ?> properties) {
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
     *
     * <p>NOTE: Not thread-safe; written for correctness; uses {@link LinkedHashMap}.</p>
     *
     * <p>SCIPIO: 2.1.0: Added.</p>
     */
    @Override
    public GeneralRuntimeException addProperties(Object... properties) {
        return addProperties(UtilMisc.orderedMap(properties));
    }

    /**
     * Makes a new exception properties map untied to the exception, for internal and client factory and code use.
     *
     * <p>NOTE: Not thread-safe; written for correctness; uses {@link LinkedHashMap} but extending classes may override
     * to change the default map type.</p>
     *
     * <p>SCIPIO: 2.1.0: Added.</p>
     */
    @Override
    public Map<String, Object> makeProperties(Map<String, ?> sourceProperties) {
        return UtilValidate.isNotEmpty(sourceProperties) ? new LinkedHashMap<>(sourceProperties) : new LinkedHashMap<>();
    }

    /**
     * Clears the general-purpose exception properties, similar to *.properties files, for passing information
     * to internal code, debugging and logging; should not be printed to public output.
     *
     * <p>NOTE: Not thread-safe; written for correctness; uses {@link LinkedHashMap}.</p>
     *
     * <p>SCIPIO: 2.1.0: Added.</p>
     */
    @Override
    public GeneralRuntimeException clearProperties() {
        this.properties = null;
        return this;
    }

    /**
     * Returns a PropertyMessage representation of the exception main detail message, which replaces the detail message
     * returned by {@link Throwable#getMessage()}.
     *
     * <p>Never returns null, even if the main detail message is null; in such case, can return a StaticPropertyMessage that returns null.</p>
     *
     * <p>Unlike {@link #getMessage()} this may not return the cause error; however, this makes it more appropriate for user-visible errors.</p>
     *
     * <p>SCIPIO: 2.x.x: Added.</p>
     */
    @Override
    public PropertyMessage getPropertyMessage() {
        if (propertyMessage != null) {
            return propertyMessage;
        } else {
            return PropertyMessage.makeFromStatic(getMessage());
        }
    }

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
    public List<PropertyMessage> getPropertyMessageList() {
        return propertyMessageList;
    }

    /**
     * Returns read-only general-purpose exception properties, similar to *.properties files, for passing information
     * to internal code, debugging and logging; should not be printed to public output.
     *
     * <p>NOTE: Not thread-safe; written for correctness; uses {@link LinkedHashMap}.</p>
     *
     * <p>SCIPIO: 2.1.0: Added.</p>
     */
    @Override
    public Map<String, Object> getProperties() {
        Map<String, Object> properties = this.properties;
        return UtilValidate.isNotEmpty(properties) ? Collections.unmodifiableMap(properties) : Collections.emptyMap();
    }

    /**
     * Returns the detail message, including the message from the nested exception if there is one.
     *
     * <p>SCIPIO: 2.x.x: modified to include the property message instead of the detail message IF it is set.</p>
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
     * Returns the list of additional messages attached to this exception, localized if possible.
     *
     * <p>NOTE: Depending on how the messages were added to the exception, these are not guaranteed
     * to be localized or may even be in a different language.</p>
     *
     * <p>SCIPIO: 2.0.0: Modified earlier to produce message list using propertyMessageList and specified locale.</p>
     *
     * @see #getPropertyMessageList()
     */
    @Override
    public List<String> getMessageList(Locale locale) {
        return PropertyMessage.getMessages(getPropertyMessageList(), locale); // SCIPIO: property messages
    }

    /**
     * Returns the list of messages attached to this exception.
     *
     * <p>SCIPIO: Use of this is now discouraged (even if caller pre-localized);
     * use {@link #getPropertyMessageList()} or {@link #getMessageList(Locale)} instead.
     * This returns only non-localized messages, unless the exception sender
     * pre-localized them. It uses the default/fallback property locale ({@link UtilProperties#getFallbackLocale()}),
     * because this fits the majority use cases.</p>
     *
     * <p>SCIPIO: 2.0.0: Modified earlier to produce message list using propertyMessageList and default locale.</p>
     *
     * @see #getMessageList(Locale)
     * @see #getPropertyMessageList()
     */
    public List<String> getMessageList() {
        return PropertyMessage.getDefPropLocaleMessages(this.propertyMessageList); // SCIPIO: property messages
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
     * If propertyMessage is set, returns it formatted in the default exception locale;
     * if not set, returns the main exception detail message instead (which may be null).
     *
     * <p>SCIPIO: 2.x.x: Added.</p>
     */
    protected String getDefExLocalePropertyOrDetailMessage() {
        if (propertyMessage != null) {
            return propertyMessage.getDefExLocaleMessage();
        } else {
            return super.getMessage();
        }
    }

}
