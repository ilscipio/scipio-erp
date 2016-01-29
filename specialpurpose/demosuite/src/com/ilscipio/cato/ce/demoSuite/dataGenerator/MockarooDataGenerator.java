package com.ilscipio.cato.ce.demoSuite.dataGenerator;

import java.net.MalformedURLException;
import java.net.URL;

public class MockarooDataGenerator extends ExternalDataGeneratorProvider {

    public MockarooDataGenerator(String exportFormat) {
        super("mockaroo", exportFormat);
        try {
            url = new URL(properties.getProperty("demosuite.test.data.provider.mockaroo.url"));
        } catch (MalformedURLException e) {
        }
        exportFormats = properties.getProperty("demosuite.test.data.provider.mockaroo.exportFormats").split(",");
        method = properties.getProperty("demosuite.test.data.provider.mockaroo.method");

    }

    // URL url = new
    // URL("http://www.mockaroo.com/api/generate.json?key=abcd1234");
    // HttpURLConnection conn = (HttpURLConnection) url.openConnection();
    // conn.setDoOutput(true);
    // conn.setRequestMethod("POST");
    // conn.setRequestProperty("Content-Type", "application/json");
    //
    // JSONObject yearsEmployed = new JSONObject();
    // yearsEmployed.put("name", "yearsEmployed");
    // yearsEmployed.put("type", "Number");
    // yearsEmployed.put("min", 1);
    // yearsEmployed.put("max", 30);
    // yearsEmployed.put("decimals", 0);
    //
    // JSONObject department = new JSONObject();
    // department.put("name", "department");
    // department.put("type", "Custom List");
    // JSONArray values = new JSONArray();
    // values.put("R+D");
    // values.put("Marketing");
    // values.put("HR");
    // department.put("values", values);
    //
    // JSONObject dob = new JSONObject();
    // dob.put("name", "dob");
    // dob.put("type", "Date");
    // dob.put("min", "1/1/1950");
    // dob.put("max", "1/1/2000");
    // dob.put("format", "%m/%d/%Y");
    //
    // JSONArray fields = new JSONArray();
    // fields.put(yearsEmployed);
    // fields.put(department);
    // fields.put(dob);
    //
    // OutputStream os = conn.getOutputStream();
    // os.write(fields.toString().getBytes());
    // os.flush();
    //
    // JSONObject data = new
    // JSONObject(IOUtils.toString(conn.getInputStream()));
    //
    // System.out.println(data.getInt("yearsEmployed"));
    // System.out.println(data.getString("department"));
    // System.out.println(data.getString("dob"));

}
