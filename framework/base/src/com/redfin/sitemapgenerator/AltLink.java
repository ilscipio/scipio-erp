package com.redfin.sitemapgenerator;

public class AltLink { // SCIPIO: 3.0.0: Added
    String url;
    String rel;
    String lang;
    String namespace;

    public AltLink(String url) {
        this.url = url;
    }

    public AltLink rel(String rel) {
        this.rel = rel;
        return this;
    }

    public AltLink lang(String lang) {
        this.lang = lang;
        return this;
    }

    public AltLink namespace(String namespace) {
        this.namespace = namespace;
        return this;
    }
}
