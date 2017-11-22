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
import java.util.List;
import java.util.Locale;

/**
 * Base OFBiz Exception, provides nested exceptions, etc
 * <p>
 * SCIPIO: 2017-11-22: Modified to implement PropertyMessageEx and support a localized main detail
 * message in addition to a separate message list (DEV NOTE: derived from exception enhancements used in cms).
 * This allows localization to be done by callers, at the correct place and time, and where otherwise impossible.
 */
@SuppressWarnings("serial")
public class GeneralException extends Exception implements PropertyMessageEx {

    public static <T> T checkException(Throwable t) throws GeneralException {
        return GeneralException.<T>checkException(t.getMessage(), t);
    }

    public static <T> T checkException(String message, Throwable t) throws GeneralException {
        if (t instanceof Error) throw (Error) t;
        if (t instanceof RuntimeException) throw (RuntimeException) t;
        if (t instanceof GeneralException) throw (GeneralException) t;
        throw (GeneralException) new GeneralException(message).initCause(t);
    }

    /**
     * SCIPIO: a PropertyMessage version of the main exception message.
     */
    private PropertyMessage propertyMessage = null;
    /**
     * Additional messages, that work in service-like manner, in ADDITION to the main exception message.
     * The main exception message is NOT counted in this.
     * SCIPIO: Modified to contain PropertyMessage instances instead of Strings.
     * NOTE: we also make this safer by always wrapping in a new ArrayList that caller can't mess up.
     * Added 2017-11-21.
     */
    private List<PropertyMessage> messages = null;

    /**
     * Creates new <code>GeneralException</code> without detail message.
     */
    public GeneralException() {
        super();
    }

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
     * @param messages error message list.
     */
    public GeneralException(String msg, List<?> messages) {
        super(msg);
        this.messages = makePropMsgList(messages); // SCIPIO: make property messages
    }

    /**
     * Constructs an <code>GeneralException</code> with the specified detail message, list and nested Exception.
     * SCIPIO: 2017-11-21: messages can be either pre-localized or hardcoded Strings or PropertyMessage instances.
     * @param msg the detail message.
     * @param messages error message list.
     * @param nested the nexted exception
     */
    public GeneralException(String msg, List<?> messages, Throwable nested) {
        super(msg, nested);
        this.messages = makePropMsgList(messages); // SCIPIO: make property messages
    }

    /**
     * Constructs an <code>GeneralException</code> with the specified detail message list and nested Exception.
     * SCIPIO: 2017-11-21: messages can be either pre-localized or hardcoded Strings or PropertyMessage instances.
     * @param messages error message list.
     * @param nested the nested exception.
     */
    public GeneralException(List<?> messages, Throwable nested) {
        super(nested);
        this.messages = makePropMsgList(messages); // SCIPIO: make property messages
    }

    /**
     * Constructs an <code>GeneralException</code> with the specified detail message list.
     * SCIPIO: 2017-11-21: messages can be either pre-localized or hardcoded Strings or PropertyMessage instances.
     * @param messages error message list.
     */
    public GeneralException(List<?> messages) {
        super();
        this.messages = makePropMsgList(messages); // SCIPIO: make property messages
    }
    
    /**
     * SCIPIO: Setter for property message.
     * Workaround for massive constructor inheritance.
     * Returns GeneralException so that can be easily chained in a throw statement.
     */
    public GeneralException setPropertyMessage(PropertyMessage propertyMessage) {
        this.propertyMessage = propertyMessage;
        return this;
    }
    
    /**
     * SCIPIO: Setter for message list including property messages.
     * Workaround for massive constructor inheritance.
     * Returns GeneralException so that can be easily chained in a throw statement.
     */
    public GeneralException setMessageList(List<?> messages) {
        this.messages = makePropMsgList(messages); // SCIPIO: make property messages
        return this;
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
     * <p>
     * SCIPIO: Use of this is now discouraged (even if caller pre-localized); 
     * use {@link #getPropertyMessageList()} or {@link #getMessageList(Locale)} instead.
     * This returns only non-localized messages, unless the exception sender 
     * pre-localized them. It uses the default/fallback property locale ({@link UtilProperties#getFallbackLocale()}), 
     * because this fits the majority use cases.
     */
    public List<String> getMessageList() {
        return PropertyMessage.getDefPropLocaleMessages(this.messages); // SCIPIO: property messages
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
    
    /**
     * SCIPIO: Returns the list of attached messages as PropertyMessage instances.
     * May be null.
     * This can be passed to {@link PropertyMessage} helper methods to get localized messages.
     */
    @Override
    public List<PropertyMessage> getPropertyMessageList() {
        return messages;
    }
    
    /**
     * SCIPIO: Shortcut for: <code>PropertyMessage.getMessages(e.getPropertyMessageList(), locale)</code>.
     */
    public List<String> getMessageList(Locale locale) {
        return PropertyMessage.getMessages(this.messages, locale); // SCIPIO: property messages
    }
    
    /**
     * SCIPIO: safely makes a PropertyMessage list for internal store.
     */
    protected static List<PropertyMessage> makePropMsgList(List<?> messages) {
        // NOTE: strict false, we don't want exceptions in exceptions
        return PropertyMessage.makeAutoFromList(messages, false);
    }
}

