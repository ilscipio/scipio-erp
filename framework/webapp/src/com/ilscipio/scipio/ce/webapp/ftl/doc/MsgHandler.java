package com.ilscipio.scipio.ce.webapp.ftl.doc;

/**
 * Message handler.
 * <p>
 * TODO: replace with log4j.
 */
public interface MsgHandler {

    public static final boolean DEBUG = false;

    public void logInfo(String msg);
    public void logError(String msg);
    public void logDebug(String msg);
    public void logWarn(String msg);

    public static class SysOutMsgHandler implements MsgHandler {

        @Override
        public void logInfo(String msg) {
            System.out.println(msg);
        }

        @Override
        public void logError(String msg) {
            System.out.println("ERROR: " + msg);
        }

        @Override
        public void logDebug(String msg) {
            if (DEBUG) {
                System.out.println(msg);
            }
        }

        @Override
        public void logWarn(String msg) {
            System.out.println("WARN: " + msg);
        }

    }

    public static class VoidMsgHandler implements MsgHandler {

        @Override
        public void logInfo(String msg) {
        }

        @Override
        public void logError(String msg) {
        }

        @Override
        public void logDebug(String msg) {
        }

        @Override
        public void logWarn(String msg) {
        }

    }
}