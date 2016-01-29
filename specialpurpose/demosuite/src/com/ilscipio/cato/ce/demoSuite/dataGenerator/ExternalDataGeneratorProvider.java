package com.ilscipio.cato.ce.demoSuite.dataGenerator;

import java.io.IOException;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.Properties;

import org.ofbiz.base.util.Debug;

public abstract class ExternalDataGeneratorProvider {
    private String provider;
    
    protected String method;
    protected URL url;
    protected String[] exportFormats;
    
    Properties properties = null;
    
    public ExternalDataGeneratorProvider(String provider, String exportFormat) {
        this.provider = provider;
    }

    protected void load() {
    }

    public void connect() {
        HttpURLConnection conn;
        try {
            conn = (HttpURLConnection) url.openConnection();
            conn.setDoOutput(true);
            conn.setRequestMethod(method);
            
             OutputStream os = conn.getOutputStream();
//             os.write(fields.toString().getBytes());
             os.flush();
        } catch (IOException e) {
            Debug.logError("Error connecting to external data generator" + e, "");
        }

    }
}
