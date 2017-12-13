package com.ilscipio.scipio.accounting.datev;

public class DatevException extends Exception {

    private static final long serialVersionUID = -5965872197826715315L;

    final DATEV_ERROR_TYPE type;

    public enum DATEV_ERROR_TYPE {
        FATAL, WARNING, SKIPPABLE
    }

    public DatevException(DATEV_ERROR_TYPE type, String message) {
        super(message);
        this.type = type;
    }

    public DatevException(DATEV_ERROR_TYPE type, Throwable t) {
        super(t);
        this.type = type;
    }

    public DatevException(DATEV_ERROR_TYPE type, String message, Throwable t) {
        super(message, t);
        this.type = type;
    }

    public DATEV_ERROR_TYPE getDatevErrorType() {
        return type;
    }

}
