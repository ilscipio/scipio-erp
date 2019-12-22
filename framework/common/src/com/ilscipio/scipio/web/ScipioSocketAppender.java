package com.ilscipio.scipio.web;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.locks.*;
import org.apache.logging.log4j.core.*;
import org.apache.logging.log4j.core.appender.AbstractAppender;
import org.apache.logging.log4j.core.appender.AppenderLoggingException;
import org.apache.logging.log4j.core.config.Property;
import org.apache.logging.log4j.core.config.plugins.*;
import org.apache.logging.log4j.core.layout.PatternLayout;
import org.ofbiz.base.lang.JSON;

@Plugin(name="ScipioSocketAppender", category="Core", elementType="appender", printObject=true)
public final class ScipioSocketAppender extends AbstractAppender {

    private final ReadWriteLock rwLock = new ReentrantReadWriteLock();
    private final Lock readLock = rwLock.readLock();
    private String channel = "log";

    protected ScipioSocketAppender(String name, Filter filter,
                                   Layout<? extends Serializable> layout, final boolean ignoreExceptions, String channel) {
        super(name, filter, layout, ignoreExceptions);
        this.channel=channel;
    }


    @Override
    public void append(LogEvent event) {
        readLock.lock();
        try {
            final byte[] bytes = getLayout().toByteArray(event);
            String message = new String(bytes);
            Map logMessage = new HashMap();
            logMessage.put("message",message);
            logMessage.put("type",event.getLevel());
            JSON obj = JSON.from(logMessage);
            SocketSessionManager.broadcastToChannel(obj.toString(),channel);
        } catch (Exception ex) {
            if (!ignoreExceptions()) {
                throw new AppenderLoggingException(ex);
            }
        } finally {
            readLock.unlock();
        }
    }

    @PluginFactory
    public static ScipioSocketAppender createAppender(
            @PluginAttribute("name") String name,
            @PluginElement("PatternLayout") Layout<? extends Serializable> layout,
            @PluginElement("Filter") final Filter filter,
            @PluginAttribute("channel") String channel) {
        if (name == null) {
            LOGGER.error("No name provided");
            return null;
        }
        if (layout == null) {
            layout = PatternLayout.createDefaultLayout();
        }
        return new ScipioSocketAppender(name, filter, layout, true,channel);
    }
}