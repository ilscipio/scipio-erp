package com.ilscipio.scipio.ce.webapp.ftl.doc;

@SuppressWarnings("serial")
public class FtlDocException extends Exception {

    public FtlDocException() {
    }

    public FtlDocException(String message) {
        super(message);
    }

    public FtlDocException(Throwable cause) {
        super(cause);
    }

    public FtlDocException(String message, Throwable cause) {
        super(message, cause);
    }


    public static class ParseException extends FtlDocException {

        public ParseException() {
            super();
        }

        public ParseException(String message, Throwable cause) {
            super(message, cause);
        }

        public ParseException(String message) {
            super(message);
        }

        public ParseException(Throwable cause) {
            super(cause);
        }
    }
}
