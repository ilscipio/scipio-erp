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
package com.ilscipio.scipio.acp;

import java.io.IOException;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import javax.crypto.Mac;
import javax.crypto.spec.SecretKeySpec;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;

/**
 * SCIPIO: 4.0.0: JSON, money and HMAC helpers of ACP. Amounts are integers in the minor unit of the currency.
 */
public final class AcpJson {

    public static final ObjectMapper MAPPER = new ObjectMapper()
            .setSerializationInclusion(JsonInclude.Include.NON_NULL)
            .configure(DeserializationFeature.USE_BIG_DECIMAL_FOR_FLOATS, true);

    private static final TypeReference<LinkedHashMap<String, Object>> MAP = new TypeReference<LinkedHashMap<String, Object>>() {};

    private AcpJson() {}

    /** Parses a JSON object; an empty text is an empty map. Throws for anything that is not an object. */
    public static Map<String, Object> parse(String text) throws IOException {
        if (text == null || text.trim().isEmpty()) return new LinkedHashMap<>();
        return MAPPER.readValue(text, MAP);
    }

    public static String write(Object o) {
        try {
            return MAPPER.writeValueAsString(o);
        } catch (IOException e) {
            throw new IllegalStateException(e);
        }
    }

    @SuppressWarnings("unchecked")
    public static Map<String, Object> map(Object o) {
        return o instanceof Map ? (Map<String, Object>) o : null;
    }

    @SuppressWarnings("unchecked")
    public static List<Object> list(Object o) {
        return o instanceof List ? (List<Object>) o : Collections.emptyList();
    }

    public static String str(Map<String, Object> m, String key) {
        Object v = m == null ? null : m.get(key);
        return v == null ? null : v.toString();
    }

    /** The ISO 4217 minor unit digits (JPY 0, KWD 3, else 2). */
    public static int minorDigits(String currency) {
        try {
            int d = java.util.Currency.getInstance(currency.toUpperCase(java.util.Locale.ROOT)).getDefaultFractionDigits();
            return d < 0 ? 2 : d;
        } catch (IllegalArgumentException | NullPointerException e) {
            return 2;
        }
    }

    public static long toMinor(BigDecimal amount, String currency) {
        if (amount == null) return 0L;
        return amount.movePointRight(minorDigits(currency)).setScale(0, RoundingMode.HALF_UP).longValueExact();
    }

    public static BigDecimal fromMinor(long minor, String currency) {
        return BigDecimal.valueOf(minor).movePointLeft(minorDigits(currency));
    }

    public static byte[] hmacSha256(String key, String text) {
        try {
            Mac mac = Mac.getInstance("HmacSHA256");
            mac.init(new SecretKeySpec(key.getBytes(StandardCharsets.UTF_8), "HmacSHA256"));
            return mac.doFinal(text.getBytes(StandardCharsets.UTF_8));
        } catch (java.security.GeneralSecurityException e) {
            throw new IllegalStateException(e);
        }
    }

    public static String hex(byte[] b) {
        StringBuilder sb = new StringBuilder(b.length * 2);
        for (byte x : b) sb.append(String.format("%02x", x));
        return sb.toString();
    }

    public static String sha256Hex(String text) {
        try {
            return hex(MessageDigest.getInstance("SHA-256").digest(text.getBytes(StandardCharsets.UTF_8)));
        } catch (NoSuchAlgorithmException e) {
            throw new IllegalStateException(e);
        }
    }

    public static boolean constantTimeEquals(String a, String b) {
        if (a == null || b == null) return false;
        return MessageDigest.isEqual(a.getBytes(StandardCharsets.UTF_8), b.getBytes(StandardCharsets.UTF_8));
    }
}
