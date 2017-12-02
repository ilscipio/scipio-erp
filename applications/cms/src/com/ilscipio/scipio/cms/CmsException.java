package com.ilscipio.scipio.cms;

import java.util.List;
import java.util.Locale;

import org.ofbiz.base.util.PropertyMessage;
import org.ofbiz.base.util.PropertyMessageEx;

/**
 * Base CMS exception.
 * <p>
 * 2016: Supports PropertyMessage for delayed localization of error message;
 * code that catches can call {@link #getPropertyMessage} and
 * pass it a locale to localize the message.
 */
@SuppressWarnings("serial")
public class CmsException extends RuntimeException implements PropertyMessageEx {

    protected final PropertyMessage propMsg;
    
    public CmsException(String msg, Throwable e) {
        super(msg, e);
        this.propMsg = PropertyMessage.makeFromStatic(msg);
    }
    
    public CmsException(String msg) {
        super(msg);
        this.propMsg = PropertyMessage.makeFromStatic(msg);
    }
    
    public CmsException(Throwable e) {
        super(e);
        this.propMsg = PropertyMessage.makeFromException(e);
    }
    
    public CmsException(String msg, PropertyMessage propMsg, Throwable e) {
        super(msg, e);
        this.propMsg = propMsg;
    }
    
    public CmsException(String msg, PropertyMessage propMsg) {
        super(msg);
        this.propMsg = propMsg;
    }
    
    public CmsException(PropertyMessage propMsg, Throwable e) {
        super(propMsg.getMessage(Locale.ENGLISH), e); // NOTE: ex msg is always ENGLISH, not Locale.getDefault()
        this.propMsg = propMsg;
    }
    
    public CmsException(PropertyMessage propMsg) {
        super(propMsg.getMessage(Locale.ENGLISH)); // NOTE: ex msg is always ENGLISH, not Locale.getDefault()
        this.propMsg = propMsg;
    }
    
    public PropertyMessage getPropertyMessage() {
        return propMsg;
    }
    
    public String getFormattedPropertyMessage() {
        return propMsg.getDefPropLocaleMessage();
    }
    
    public String getFormattedPropertyMessage(Locale locale) {
        return propMsg.getMessage(locale);
    }

    @Override
    public List<PropertyMessage> getPropertyMessageList() {
        // TODO
        return null;
    }

}
