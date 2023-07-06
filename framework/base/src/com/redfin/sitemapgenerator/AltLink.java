package com.redfin.sitemapgenerator;

public class AltLink { // SCIPIO: 3.0.0: Added
    String url;
    String rel;
    String lang;
    String namespace;

    public AltLink(String url) {
        this.url = url;
    }

    public String url() {
        return url;
    }

    public AltLink rel(String rel) {
        this.rel = rel;
        return this;
    }

    public String rel() {
        return rel;
    }

    public AltLink lang(String lang) {
        this.lang = lang;
        return this;
    }

    public String lang() {
        return lang;
    }

    public AltLink namespace(String namespace) {
        this.namespace = namespace;
        return this;
    }

    public String namespace() {
        return namespace;
    }

    public String toLangUrlString() {
        return lang() + "=" + url();
    }
}
