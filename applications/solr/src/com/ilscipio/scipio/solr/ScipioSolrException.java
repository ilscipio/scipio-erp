package com.ilscipio.scipio.solr;

/**
 * Scipio Solr Exception.
 * <p>
 * NOTE: 2018-02-21: This is not currently in use much.
 */
@SuppressWarnings("serial")
public class ScipioSolrException extends Exception {

    private boolean lightweight = false;
    
    public ScipioSolrException() {
    }

    public ScipioSolrException(String message) {
        super(message);
    }

    public ScipioSolrException(Throwable cause) {
        super(cause);
    }

    public ScipioSolrException(String message, Throwable cause) {
        super(message, cause);
    }

    public boolean isLightweight() {
        return lightweight;
    }

    public ScipioSolrException setLightweight(boolean lightweight) {
        this.lightweight = lightweight;
        return this;
    }
}
