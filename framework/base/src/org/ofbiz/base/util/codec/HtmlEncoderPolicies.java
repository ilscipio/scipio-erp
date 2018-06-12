/*******************************************************************************
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 *******************************************************************************/
package org.ofbiz.base.util.codec;

import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilCodec;
import org.ofbiz.base.util.UtilCodec.OwaspHtmlEncoder;
import org.ofbiz.base.util.UtilCodec.SimpleEncoder;
import org.owasp.html.PolicyFactory;
import org.owasp.html.Sanitizers;

/**
 * SCIPIO: HTML encoder factories for use with owasp.properties.
 * Added 2018-06-11.
 */
public abstract class HtmlEncoderPolicies {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private HtmlEncoderPolicies() {}

    public static abstract class OwaspEncoderFactory implements EncoderFactory {
        @Override
        public SimpleEncoder createEncoder(Map<String, String> config, EncoderSource encoderSource) {
            return new UtilCodec.FixedOwaspHtmlEncoder(createPolicy(config, encoderSource));
        }

        public abstract PolicyFactory createPolicy(Map<String, String> config, EncoderSource encoderSource);

        public static PolicyFactory getOtherPolicy(String name, EncoderSource encoderSource) {
            SimpleEncoder encoder = encoderSource.getEncoder(name);
            if (encoder != null && encoder instanceof OwaspHtmlEncoder) {
                return ((OwaspHtmlEncoder) encoder).getSanitizationPolicy();
            }
            return null;
        }
    }

    public static class StrictOwaspPolicy extends OwaspEncoderFactory {
        public static final PolicyFactory DEFAULT_STRICT_POLICY =
                Sanitizers.FORMATTING.and(Sanitizers.BLOCKS).and(Sanitizers.IMAGES).and(Sanitizers.LINKS).and(Sanitizers.STYLES);

        @Override
        public PolicyFactory createPolicy(Map<String, String> config, EncoderSource encoderSource) {
            return DEFAULT_STRICT_POLICY;
        }
    }

    public static class PermissiveOwaspPolicy extends OwaspEncoderFactory {
        public static final PolicyFactory DEFAULT_PERMISSIVE_POLICY =
                StrictOwaspPolicy.DEFAULT_STRICT_POLICY.and(UtilCodec.HtmlEncoder.PERMISSIVE_POLICY);
        @Override
        public PolicyFactory createPolicy(Map<String, String> config, EncoderSource encoderSource) {
            return DEFAULT_PERMISSIVE_POLICY;
        }
    }

    public static class DefaultOwaspPolicy extends OwaspEncoderFactory {
        @Override
        public PolicyFactory createPolicy(Map<String, String> config, EncoderSource encoderSource) {
            String sourceName = UtilCodec.OwaspHtmlEncoder.PERMISSIVE ? "html-perm" : "html-strict";
            PolicyFactory pf = getOtherPolicy(sourceName, encoderSource);
            if (pf == null) {
                Debug.logError("Error in owasp properties configuration:"
                        + " default policy references missing or incompatible '"
                        + sourceName + "' policy; defaulting to strict policy", module);
                pf = StrictOwaspPolicy.DEFAULT_STRICT_POLICY;
            }
            return pf;
        }
    }

}
