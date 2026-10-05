/*
 * Scipio Commerce
 * Copyright (C) Ilscipio GmbH
 *
 * This file is part of Scipio Commerce. Scipio Commerce is free software: you
 * can redistribute it and modify it under the terms of the GNU Affero General
 * Public License, version 3, as published by the Free Software Foundation.
 * Scipio Commerce is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
 * for more details. You should have received a copy of the license with this
 * work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
 * A commercial license is available from Ilscipio GmbH.
 *
 * SPDX-License-Identifier: AGPL-3.0-only
 */
package org.ofbiz.base.util.codec;

import java.util.Map;

import org.ofbiz.base.util.UtilCodec;
import org.ofbiz.base.util.UtilCodec.HtmlEncoder;
import org.ofbiz.base.util.UtilCodec.OwaspHtmlEncoder;
import org.ofbiz.base.util.UtilCodec.SimpleEncoder;
import org.owasp.html.PolicyFactory;
import org.owasp.html.Sanitizers;

/**
 * SCIPIO: HTML encoder factories for use with utilcodec.properties.
 * Added 2018-06-11.
 */
public abstract class HtmlSanitizerPolicies {
    //private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private HtmlSanitizerPolicies() {}

    /*
     * ***************************************************
     * Generic factories
     * ***************************************************
     */

    public static class NoSanitizeHtmlEncoder extends HtmlEncoder {
        @Override
        public String sanitize(String original) {
            return original;
        }
    }

    /**
     * Returns a basic HtmlEncoder that does not sanitize!
     */
    public static class NoSanitizerPolicy implements EncoderFactory {
        @Override
        public SimpleEncoder createEncoder(String name, Map<String, String> config, EncoderSource encoderSource) {
            return new NoSanitizeHtmlEncoder();
        }
    }

    /**
     * Returns a basic HtmlEncoder that does no sanitization, just encoding.
     */
    public static class EscapeSanitizerPolicy implements EncoderFactory {
        @Override
        public SimpleEncoder createEncoder(String name, Map<String, String> config, EncoderSource encoderSource) {
            return new UtilCodec.EscapeHtmlEncoder();
        }
    }

    /**
     * Returns a basic HtmlEncoder that does no sanitization, just encoding.
     */
    public static class AliasSanitizerPolicy implements EncoderFactory {
        @Override
        public SimpleEncoder createEncoder(String name, Map<String, String> config, EncoderSource encoderSource) {
            String alias = config.get("alias");
            SimpleEncoder encoder = encoderSource.getEncoder(alias);
            if (encoder == null) {
                throw new IllegalArgumentException("cannot find encoder for alias '" + alias + "'");
            }
            return encoder;
        }
    }

    /**
     * Returns html-perm or html-strict depending on sanitizer.permissive.policy.
     */
    public static class PermOrStrictSanitizerPolicy implements EncoderFactory {
        @Override
        public SimpleEncoder createEncoder(String name, Map<String, String> config, EncoderSource encoderSource) {
            String sourceName = UtilCodec.OwaspHtmlEncoder.PERMISSIVE ? "html-perm" : "html-strict";
            SimpleEncoder encoder = encoderSource.getEncoder(sourceName);
            if (encoder == null) {
                throw new IllegalArgumentException("Cannot find encoder for name '" + sourceName + "'");
            }
            return encoder;
        }
    }

    /*
     * ***************************************************
     * OWASP policy factories
     * ***************************************************
     */

    public static abstract class OwaspSanitizerFactory implements EncoderFactory {
        @Override
        public SimpleEncoder createEncoder(String name, Map<String, String> config, EncoderSource encoderSource) {
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

    public static class StrictOwaspPolicy extends OwaspSanitizerFactory {
        public static final PolicyFactory DEFAULT_STRICT_POLICY =
                Sanitizers.FORMATTING.and(Sanitizers.BLOCKS).and(Sanitizers.IMAGES).and(Sanitizers.LINKS).and(Sanitizers.STYLES);

        @Override
        public PolicyFactory createPolicy(Map<String, String> config, EncoderSource encoderSource) {
            return DEFAULT_STRICT_POLICY;
        }
    }

    public static class PermissiveOwaspPolicy extends OwaspSanitizerFactory {
        public static final PolicyFactory DEFAULT_PERMISSIVE_POLICY =
                StrictOwaspPolicy.DEFAULT_STRICT_POLICY.and(UtilCodec.HtmlEncoder.PERMISSIVE_POLICY);
        @Override
        public PolicyFactory createPolicy(Map<String, String> config, EncoderSource encoderSource) {
            return DEFAULT_PERMISSIVE_POLICY;
        }
    }

}
