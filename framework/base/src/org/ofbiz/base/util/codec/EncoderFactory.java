package org.ofbiz.base.util.codec;

import java.util.Map;

import org.ofbiz.base.util.UtilCodec.SimpleEncoder;

/**
 * SCIPIO: for use with utilcodec.properties.
 * Added 2018-06-11.
 */
public interface EncoderFactory {

    SimpleEncoder createEncoder(String name, Map<String, String> config, EncoderSource encoderSource);

    /**
     * Provides access to other encoders during construction.
     */
    interface EncoderSource {
        SimpleEncoder getEncoder(String name);
        Map<String, String> getEncoderConfig(String name);
    }

}
