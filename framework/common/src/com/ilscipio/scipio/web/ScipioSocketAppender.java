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
import org.apache.logging.log4j.core.config.Property;
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

    private final String channel; // = "log"
    private final int bufferMaxBytes = UtilProperties.getPropertyAsInteger("catalina", "webSocket.log.appender.buffer.maxBytes", 16384);
    private final int bufferMaxWait = UtilProperties.getPropertyAsInteger("catalina", "webSocket.log.appender.buffer.maxWait", 500); // milliseconds
    private final int bufferMinMsgCount = UtilProperties.getPropertyAsInteger("catalina", "webSocket.log.appender.buffer.minMsgCount", 2048); // initial buffer size

    private List<MessageEntry> bufferMessages = new ArrayList<>(bufferMinMsgCount);
    private int bufferBytes = 0;
    private long bufferLastFlushTime = System.currentTimeMillis();
    private final Object bufferLock = new Object();

    // Likely not helping
    //private final ReadWriteLock rwLock = new ReentrantReadWriteLock();
    //private final Lock readLock = rwLock.readLock();

    protected ScipioSocketAppender(String name, Filter filter,
                                   Layout<? extends Serializable> layout, final boolean ignoreExceptions, String channel) {
        super(name, filter, layout, ignoreExceptions, Property.EMPTY_ARRAY);
        this.channel = channel;
    }

    @Override
    public void append(LogEvent event) {
        byte[] messageBytes = getLayout().toByteArray(event);
        MessageEntry messageEntry = new MessageEntry(messageBytes, event.getLevel());
        long nowTime = System.currentTimeMillis();
        List<MessageEntry> flushMessages;
        synchronized(bufferLock) { // TODO: REVIEW: maybe faster structure than synchronized to accumulate messages is possible...
            bufferMessages.add(messageEntry);
            bufferBytes += messageBytes.length;
            if ((bufferBytes < bufferMaxBytes) && ((nowTime - bufferLastFlushTime) < bufferMaxWait)) {
                return;
            }
            bufferBytes = 0;
            bufferLastFlushTime = nowTime;
            flushMessages = bufferMessages;
            bufferMessages = new ArrayList<>(bufferMinMsgCount);
        }
        // NOTE: Technically the below should be within synchronized, but it will slow down appends, and this alone is unlikely to result in too many ordering changes.
        try {
            List<Map<String, Object>> messageList = new ArrayList<>(flushMessages.size());
            for (MessageEntry entry : flushMessages) {
                messageList.add(entry.toMap());
            }
            Map<String, Object> dataMap = new HashMap<>();
            dataMap.put("messageList", messageList);
            JSON dataMapJson = JSON.from(dataMap);
            SocketSessionManager ssm = SocketSessionManager.getDefault();
            try {
                ssm.allowLogging.set(false); // SPECIAL: Don't let this log anything or else endless logging
                ssm.broadcastToChannel(dataMapJson.toString(), channel);
            } finally {
                ssm.allowLogging.remove();
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
        final byte[] bytes;
        final Level level;

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