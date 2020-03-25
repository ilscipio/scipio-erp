package com.ilscipio.scipio.web;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.google.common.base.Charsets;
import org.apache.logging.log4j.Level;
import org.apache.logging.log4j.core.*;
import org.apache.logging.log4j.core.appender.AbstractAppender;
import org.apache.logging.log4j.core.appender.AppenderLoggingException;
import org.apache.logging.log4j.core.config.plugins.*;
import org.apache.logging.log4j.core.layout.PatternLayout;
import org.ofbiz.base.lang.JSON;
import org.ofbiz.base.util.UtilProperties;

/**
 * ScipioSocketAppender.
 * <p>
 *  TODO: REVIEW: this could maybe profit from a dedicated thread to eat buffered messages and send to websocket, to remove all burden possible from the calling thread...
 *   needs depends on the implementations of both log4j and websockets
 * </p>
 */
@Plugin(name="ScipioSocketAppender", category="Core", elementType="appender", printObject=true)
public final class ScipioSocketAppender extends AbstractAppender {

    private String channel = "log";
    private final List<MessageEntry> bufferMessages = new ArrayList<>();
    private int bufferBytes = 0;
    private long bufferLastFlushTime = System.currentTimeMillis();
    private int bufferMaxBytes = UtilProperties.getPropertyAsInteger("catalina", "webSocket.log.appender.buffer.maxBytes", 16384);
    private int bufferMaxWait = UtilProperties.getPropertyAsInteger("catalina", "webSocket.log.appender.buffer.maxWait", 500); // milliseconds
    // Likely not helping
    //private final ReadWriteLock rwLock = new ReentrantReadWriteLock();
    //private final Lock readLock = rwLock.readLock();

    protected ScipioSocketAppender(String name, Filter filter,
                                   Layout<? extends Serializable> layout, final boolean ignoreExceptions, String channel) {
        super(name, filter, layout, ignoreExceptions); // FIXME: deprecated
        this.channel = channel;
    }

    @Override
    public void append(LogEvent event) {
        List<Map<String, Object>> messageList;
        synchronized(bufferMessages) { // TODO: REVIEW: maybe faster structure than synchronized to accumulate messages is possible...
            byte[] messageBytes = getLayout().toByteArray(event);
            bufferMessages.add(new MessageEntry(messageBytes, event.getLevel()));
            bufferBytes += messageBytes.length;
            long nowTime = System.currentTimeMillis();
            if ((bufferBytes < bufferMaxBytes) && ((nowTime - bufferLastFlushTime) < bufferMaxWait)) {
                return;
            }
            messageList = new ArrayList<>(bufferMessages.size());
            for (MessageEntry entry : bufferMessages) {
                messageList.add(entry.toMap());
            }
            bufferMessages.clear();
            bufferBytes = 0;
            bufferLastFlushTime = nowTime;
        }
        // NOTE: technically the below should be within synchronized, but highly unlikely to result in ordering changes
        try {
            Map<String, Object> json = new HashMap<>();
            json.put("messageList", messageList);
            JSON obj = JSON.from(json);
            try {
                SocketSessionManager.allowLogging.set(false); // SPECIAL: Don't let this log anything or else endless logging
                SocketSessionManager.broadcastToChannel(obj.toString(), channel);
            } finally {
                SocketSessionManager.allowLogging.remove();
            }
        } catch (Exception ex) {
            if (!ignoreExceptions()) {
                throw new AppenderLoggingException(ex);
            }
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

    private static class MessageEntry {
        byte[] bytes;
        Level level;

        public MessageEntry(byte[] bytes, Level level) {
            this.bytes = bytes;
            this.level = level;
        }

        public Map<String, Object> toMap() {
            Map<String, Object> map = new HashMap<>();
            map.put("message", new String(bytes, Charsets.UTF_8));
            map.put("level", level);
            return map;
        }
    }
}