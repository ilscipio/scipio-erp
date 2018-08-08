package com.ilscipio.scipio.ce.webapp.filter.urlrewrite;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;

import org.ofbiz.base.location.FlexibleLocation;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.cache.UtilCache;
import org.tuckey.web.filters.urlrewrite.Conf;

public abstract class UrlConfUtil {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    
    private static final UtilCache<String, Conf> pathCache = UtilCache.createUtilCache("scipio.urlrewrite.conf.path");
    
    private UrlConfUtil() {
    }
    
    public static Conf getConfFromLocation(String urlConfPath) throws IOException {
        URL confUrl;
        confUrl = FlexibleLocation.resolveLocation(urlConfPath);
        if (confUrl == null) {
            throw new IOException("Could not resolve urlrewrite conf path: " + urlConfPath);
        }
        urlConfPath = confUrl.toString();
        
        Conf conf = pathCache.get(urlConfPath);
        if (conf != null) {
            return conf;
        }

        InputStream inputStream = null;
        try {
            inputStream = confUrl.openStream();
            // attempt to retrieve from location other than local WEB-INF

            if (inputStream == null) {
                throw new IOException("Unable to find urlrewrite conf file at: " + urlConfPath);
            } else {
                conf = new Conf(null, inputStream, urlConfPath, urlConfPath, false);
            }
        } finally {
            try {
                if (inputStream != null) {
                    inputStream.close();
                }
            } catch (IOException e) {
                Debug.logError(e, module);
            }
        }
        pathCache.put(urlConfPath, conf);
        return conf;
    }

}
