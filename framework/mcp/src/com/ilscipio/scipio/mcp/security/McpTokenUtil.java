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
package com.ilscipio.scipio.mcp.security;

import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import java.util.zip.CRC32;

/**
 * SCIPIO: 4.0.0: Token format and hashing.
 *
 * <p>Raw token: {@code scp_<tokenId>_<secret>_<crc>}. tokenId is 12 base62 chars, secret is 32 random bytes in
 * base62 (43 chars), crc is 6 base62 chars of CRC32 over {@code scp_<tokenId>_<secret>} so secret scanners can
 * validate leaked tokens offline. Only SHA-256(raw token) is stored.</p>
 */
public final class McpTokenUtil {

    public static final String PREFIX = "scp_";
    private static final String ALPHABET = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
    private static final SecureRandom RANDOM = new SecureRandom();

    private McpTokenUtil() {}

    public static final class Generated {
        public final String tokenId;
        public final String rawToken;
        public final String hash;
        public final String displayPrefix;

        Generated(String tokenId, String rawToken, String hash, String displayPrefix) {
            this.tokenId = tokenId;
            this.rawToken = rawToken;
            this.hash = hash;
            this.displayPrefix = displayPrefix;
        }
    }

    public static Generated generate() {
        String tokenId = randomBase62(12);
        String secret = randomBase62(43);
        String body = PREFIX + tokenId + "_" + secret;
        String raw = body + "_" + crc(body);
        return new Generated(tokenId, raw, hash(raw), PREFIX + tokenId + "_" + secret.substring(0, 4) + "...");
    }

    /** Returns the tokenId embedded in a raw token, or null when the format or checksum is invalid. */
    public static String parseTokenId(String raw) {
        if (raw == null || !raw.startsWith(PREFIX)) return null;
        String[] parts = raw.split("_");
        if (parts.length != 4) return null;
        String body = parts[0] + "_" + parts[1] + "_" + parts[2];
        if (!crc(body).equals(parts[3])) return null;
        if (parts[1].length() != 12) return null;
        return parts[1];
    }

    public static String hash(String raw) {
        try {
            MessageDigest md = MessageDigest.getInstance("SHA-256");
            byte[] digest = md.digest(raw.getBytes(StandardCharsets.UTF_8));
            StringBuilder sb = new StringBuilder(64);
            for (byte b : digest) sb.append(String.format("%02x", b));
            return sb.toString();
        } catch (NoSuchAlgorithmException e) {
            throw new IllegalStateException(e);
        }
    }

    /** Constant-time comparison of a raw token against a stored hash. */
    public static boolean verify(String raw, String storedHash) {
        if (raw == null || storedHash == null) return false;
        byte[] a = hash(raw).getBytes(StandardCharsets.UTF_8);
        byte[] b = storedHash.getBytes(StandardCharsets.UTF_8);
        return MessageDigest.isEqual(a, b);
    }

    static String crc(String body) {
        CRC32 crc = new CRC32();
        crc.update(body.getBytes(StandardCharsets.UTF_8));
        long v = crc.getValue();
        StringBuilder sb = new StringBuilder(6);
        for (int i = 0; i < 6; i++) {
            sb.append(ALPHABET.charAt((int) (v % 62)));
            v /= 62;
        }
        return sb.toString();
    }

    static String randomBase62(int length) {
        StringBuilder sb = new StringBuilder(length);
        for (int i = 0; i < length; i++) {
            sb.append(ALPHABET.charAt(RANDOM.nextInt(ALPHABET.length())));
        }
        return sb.toString();
    }

    public static String randomSessionId() {
        byte[] bytes = new byte[32];
        RANDOM.nextBytes(bytes);
        return java.util.Base64.getUrlEncoder().withoutPadding().encodeToString(bytes);
    }
}
