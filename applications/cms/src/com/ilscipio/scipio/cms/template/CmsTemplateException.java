package com.ilscipio.scipio.cms.template;

import java.util.Locale;

import org.ofbiz.base.util.PropertyMessage;
import org.ofbiz.base.util.UtilRender;
import org.ofbiz.base.util.UtilRender.RenderExceptionMode;

import com.ilscipio.scipio.cms.CmsException;

@SuppressWarnings("serial")
public class CmsTemplateException extends CmsException implements UtilRender.RenderExceptionModeHolder {

    private PropertyMessage friendlyMsgLabel;
    private PropertyMessage friendlyMsg;
    private RenderExceptionMode renderExceptionMode = null;

    public CmsTemplateException(String msg, Throwable e) {
        super(msg, e);
    }

    public CmsTemplateException(String msg) {
        super(msg);
    }

    public CmsTemplateException(Throwable e) {
        super(e);
    }

    public CmsTemplateException(String msg, PropertyMessage propMsg, Throwable e) {
        super(msg, propMsg, e);
    }

    public CmsTemplateException(String msg, PropertyMessage propMsg) {
        super(msg, propMsg);
    }

    public CmsTemplateException(PropertyMessage propMsg, Throwable e) {
        super(propMsg, e);
    }

    public CmsTemplateException(PropertyMessage propMsg) {
        super(propMsg);
    }

    public static CmsTemplateException getFromExceptionOrCauseDeep(Throwable t) {
        while (t != null) {
            if (t instanceof CmsTemplateException) return (CmsTemplateException) t;
            t = t.getCause();
        }
        return null;
    }

    public PropertyMessage getFriendlyPropertyMessageLabel() {
        return friendlyMsgLabel;
    }

    public void setFriendlyPropertyMessageLabel(PropertyMessage friendlyMsgLabel) {
        this.friendlyMsgLabel = friendlyMsgLabel;
    }

    public String getFriendlyMessageLabel() {
        return friendlyMsgLabel != null ? friendlyMsgLabel.getDefPropLocaleMessage() : null;
    }

    public String getFriendlyMessageLabel(Locale locale) {
        return friendlyMsgLabel != null ? friendlyMsgLabel.getMessage(locale) : null;
    }

    public void setFriendlyMessageLabel(String friendlyMsgLabel) {
        this.friendlyMsgLabel = PropertyMessage.makeFromStatic(friendlyMsgLabel);
    }

    public PropertyMessage getFriendlyPropertyMessage() {
        return friendlyMsg;
    }

    public void setFriendlyPropertyMessage(PropertyMessage friendlyMsg) {
        this.friendlyMsg = friendlyMsg;
    }

    public String getFriendlyMessage() {
        return friendlyMsg != null ? friendlyMsg.getDefPropLocaleMessage() : null;
    }

    public String getFriendlyMessage(Locale locale) {
        return friendlyMsg != null ? friendlyMsg.getMessage(locale) : null;
    }

    public void setFriendlyMessage(String friendlyMsg) {
        this.friendlyMsg = PropertyMessage.makeFromStatic(friendlyMsg);
    }

    @Override
    public RenderExceptionMode getRenderExceptionMode() {
        // DEV NOTE: this could be recursive and call UtilRender.getRenderExceptionMode(getCause()),
        // but am leaving out until I figure out if it's necessary...
        return renderExceptionMode;
    }

    public void setRenderExceptionMode(RenderExceptionMode renderExMode) {
        this.renderExceptionMode = renderExMode;
    }
}
