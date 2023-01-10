package com.ilscipio.scipio.web.hotwire;

import com.ilscipio.scipio.web.WebChannelHandler;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;

import java.util.List;

public class TurboChannelHandler extends WebChannelHandler {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    private static final TurboChannelHandler DEFAULT = new TurboChannelHandler("default");
    protected static final List<WebChannelHandler> DEFAULT_LIST = UtilMisc.unmodifiableArrayList(new TurboChannelHandler("turbo"));

    public static WebChannelHandler getDefault() {
        return DEFAULT;
    }

    public static List<WebChannelHandler> getDefaultAsList() {
        return DEFAULT_LIST;
    }

    public TurboChannelHandler(String name) {
        super(name);
    }

    @Override
    public Object onMessage(Args args) {
        return false;
    }
}
