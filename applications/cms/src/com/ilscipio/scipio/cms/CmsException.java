package com.ilscipio.scipio.cms;

import java.util.Collection;
import java.util.List;
import java.util.Locale;

import org.ofbiz.base.util.PropertyMessage;
import org.ofbiz.base.util.PropertyMessageEx.SettablePropertyMessageEx;
import org.ofbiz.base.util.PropertyMessageExUtil;

/**
 * Base CMS exception.
 * <p>
 * This is essentially an unchecked version of {@link org.ofbiz.base.util.GeneralException},
 * and some of the subclasses are analogous.
 * <p>
 * 2016: Supports PropertyMessage for delayed localization of error message;
 * code that catches can call {@link #getPropertyMessage} and
 * pass it a locale to localize the message.
 */
@SuppressWarnings("serial")
public class CmsException extends RuntimeException implements SettablePropertyMessageEx {

    protected PropertyMessage propertyMessage;
    protected List<PropertyMessage> messages;

    public CmsException(String msg, Throwable e) {
        super(msg, e);
        this.propertyMessage = PropertyMessage.makeFromStatic(msg);
    }

    public CmsException(String msg) {
        super(msg);
        this.propertyMessage = PropertyMessage.makeFromStatic(msg);
    }

    public CmsException(Throwable e) {
        super(e);
        this.propertyMessage = PropertyMessage.makeFromException(e);
    }

    public CmsException(String msg, PropertyMessage propMsg, Throwable e) {
        super(msg, e);
        this.propertyMessage = propMsg;
    }

    public CmsException(String msg, PropertyMessage propMsg) {
        super(msg);
        this.propertyMessage = propMsg;
    }

    public CmsException(PropertyMessage propMsg, Throwable e) {
        super(propMsg.getDefExLocaleMessage(), e); // NOTE: ex msg is always ENGLISH, not Locale.getDefault()
        this.propertyMessage = propMsg;
    }

    public CmsException(PropertyMessage propMsg) {
        super(propMsg.getDefExLocaleMessage()); // NOTE: ex msg is always ENGLISH, not Locale.getDefault()
        this.propertyMessage = propMsg;
    }

    /**
     * Setter for property message.
     * Workaround for massive constructor inheritance.
     * Returns CmsException so that can be easily chained in a throw statement.
     */
    public CmsException setPropertyMessage(PropertyMessage propertyMessage) {
        this.propertyMessage = propertyMessage;
        return this;
    }

    /**
     * SCIPIO: Setter for message list including property messages.
     * Workaround for massive constructor inheritance.
     * Returns GeneralException so that can be easily chained in a throw statement.
     */
    @Override
    public CmsException setPropertyMessageList(Collection<?> messageList) {
        this.messages = PropertyMessageExUtil.makePropertyMessageList(messageList);
        return this;
    }

    public PropertyMessage getPropertyMessage() {
        return propertyMessage;
    }

    public String getFormattedPropertyMessage() {
        return propertyMessage.getDefPropLocaleMessage();
    }

    public String getFormattedPropertyMessage(Locale locale) {
        return propertyMessage.getMessage(locale);
    }

    @Override
    public List<PropertyMessage> getPropertyMessageList() {
        return messages;
    }
}
