package com.ilscipio.scipio.ce.webapp.filter;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.List;

import org.ofbiz.base.location.FlexibleLocation;
import org.ofbiz.base.util.Debug;
import org.tuckey.web.filters.urlrewrite.Conf;
import org.tuckey.web.filters.urlrewrite.OutboundRule;

/**
 * Wrapper to help interact with urlrewrite.xml files.
 * Avoids reloading the urlrewrite.xml file for every single URL.
 */
public class UrlRewriteConf {

    public static final String module = UrlRewriteConf.class.getName();
    
    protected final String urlConfPath;
    protected final Conf conf;
    
    protected UrlRewriteConf(String urlConfPath, Conf conf) throws IOException {
        this.urlConfPath = urlConfPath;
        this.conf = conf;
    }
        
    public static UrlRewriteConf loadConf(String urlConfPath) throws IOException {
        if (Debug.verboseOn()) {
            Debug.logVerbose("urlrewrite: loading " + urlConfPath, module);
        }
        // load config
        URL confUrl;
        InputStream inputStream = null;
        
        confUrl = FlexibleLocation.resolveLocation(urlConfPath);

        try {
            inputStream = confUrl.openStream();
            // attempt to retrieve from location other than local WEB-INF
    
            if (inputStream == null) {
                throw new IOException("Unable to find urlrewrite conf file at: " + urlConfPath);
            } else {
                Conf conf = new Conf(null, inputStream, urlConfPath, confUrl.toString(), false);
                
                return new UrlRewriteConf(urlConfPath, conf);
            }
        } finally {
            try {
                if (inputStream != null) inputStream.close();
            } catch (IOException e) {
            }
        }

    }
    
    public String getUrlConfPath() {
        return urlConfPath;
    }

    public Conf getConf() {
        return conf;
    }

    /**
     * Use urlrewritefilter rules to convert urls - emulates urlrewritefilter - just like the original url would be
     * TODO: REVIEW: 2017: urlrewritefilter internals may have changed...
     */
    public String processOutboundUrl(String originalUrl) {
        String rewriteUrl = originalUrl;

        // Taken over from urlrewritefilter
        if (Debug.verboseOn()) {
            Debug.logVerbose("urlrewrite: processing outbound url for " + originalUrl, module);
        }

        if (originalUrl == null) {
            return "";
        }

        @SuppressWarnings("unchecked")
        final List<OutboundRule> outboundRules = conf.getOutboundRules();
        for (int i = 0; i < outboundRules.size(); i++) {
            final OutboundRule outboundRule = (OutboundRule) outboundRules
                    .get(i);

            rewriteUrl = rewriteUrl.replaceAll(outboundRule.getFrom(),
                    outboundRule.getTo());

            if (rewriteUrl != null) {
                // means this rule has matched
                if (Debug.verboseOn()) {
                    Debug.logVerbose("urlrewrite: \"" + outboundRule.getDisplayName() + "\" matched", module);
                }
                if (outboundRule.isLast()) {
                    if (Debug.verboseOn()) {
                        Debug.logVerbose("urlrewrite: rule is last", module);
                    }
                    // there can be no more matches on this request
                    break;
                }
            }
        }

        return rewriteUrl;
    }

}
