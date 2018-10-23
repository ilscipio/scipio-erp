package com.ilscipio.scipio.product.seo;

import java.util.List;

import org.ofbiz.base.util.GeneralException;

@SuppressWarnings("serial")
public class SeoCatalogException extends GeneralException {

    public SeoCatalogException() {
        super();
    }

    public SeoCatalogException(List<?> messages, Throwable nested) {
        super(messages, nested);
    }

    public SeoCatalogException(List<?> messages) {
        super(messages);
    }

    public SeoCatalogException(String msg, List<?> messages, Throwable nested) {
        super(msg, messages, nested);
    }

    public SeoCatalogException(String msg, List<?> messages) {
        super(msg, messages);
    }

    public SeoCatalogException(String msg, Throwable nested) {
        super(msg, nested);
    }

    public SeoCatalogException(String msg) {
        super(msg);
    }

    public SeoCatalogException(Throwable nested) {
        super(nested);
    }

}