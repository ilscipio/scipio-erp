package com.ilscipio.scipio.accounting.external.datev;

public class DatevException extends Exception {
    private static final long serialVersionUID = -5965872197826715315L;

    final String message;

    public DatevException() {
        super();
        this.message = null;
    }

    public DatevException(String message) {
        super(message);
        this.message = message;
    }

    public DatevException(String message, Throwable t) {
        super(message, t);
        this.message = message;
    }

    @Override
    public String getMessage() {
        return message;
    }

}
