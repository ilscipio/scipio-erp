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
package org.ofbiz.entity.tenant;

import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.InvalidKeyException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.time.Duration;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;

import javax.crypto.Mac;
import javax.crypto.spec.SecretKeySpec;

/**
 * SCIPIO: 4.0.0: Pooled runtime: {@link TenantStorage} on an S3-compatible object storage (Hetzner Object Storage,
 * MinIO, AWS S3). Requests are signed with AWS Signature Version 4; path-style URLs
 * ({@code <endpoint>/<bucket>/<key>}). No SDK: java.net.http only.
 */
public class S3TenantStorage implements TenantStorage {

    private static final String EMPTY_SHA256 = "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855";
    private static final DateTimeFormatter AMZ_DATE = DateTimeFormatter.ofPattern("yyyyMMdd'T'HHmmss'Z'");
    private static final DateTimeFormatter AMZ_DAY = DateTimeFormatter.ofPattern("yyyyMMdd");

    private final URI endpoint;
    private final String region;
    private final String bucket;
    private final String accessKey;
    private final String secretKey;
    private final HttpClient client;

    public S3TenantStorage(String endpoint, String region, String bucket, String accessKey, String secretKey) {
        this.endpoint = URI.create(endpoint.endsWith("/") ? endpoint.substring(0, endpoint.length() - 1) : endpoint);
        this.region = region;
        this.bucket = bucket;
        this.accessKey = accessKey;
        this.secretKey = secretKey;
        this.client = HttpClient.newBuilder().connectTimeout(Duration.ofSeconds(10)).build();
    }

    @Override
    public void put(String key, Path file, String contentType) throws IOException {
        byte[] body = Files.readAllBytes(file);
        HttpResponse<String> response = send("PUT", objectPath(key), body, contentType, HttpResponse.BodyHandlers.ofString());
        if (response.statusCode() / 100 != 2) {
            throw new IOException("S3 PUT " + key + ": HTTP " + response.statusCode() + " " + abbreviate(response.body()));
        }
    }

    @Override
    public InputStream get(String key) throws IOException {
        HttpResponse<InputStream> response = send("GET", objectPath(key), null, null, HttpResponse.BodyHandlers.ofInputStream());
        if (response.statusCode() == 404) {
            response.body().close();
            return null;
        }
        if (response.statusCode() / 100 != 2) {
            String text = new String(response.body().readAllBytes(), StandardCharsets.UTF_8);
            throw new IOException("S3 GET " + key + ": HTTP " + response.statusCode() + " " + abbreviate(text));
        }
        return response.body();
    }

    @Override
    public void delete(String key) throws IOException {
        HttpResponse<String> response = send("DELETE", objectPath(key), null, null, HttpResponse.BodyHandlers.ofString());
        if (response.statusCode() / 100 != 2 && response.statusCode() != 404) {
            throw new IOException("S3 DELETE " + key + ": HTTP " + response.statusCode() + " " + abbreviate(response.body()));
        }
    }

    /** Creates the bucket (for tests and first setup); no error when it exists and belongs to the caller. */
    public void createBucket() throws IOException {
        HttpResponse<String> response = send("PUT", "/" + encode(bucket), new byte[0], null, HttpResponse.BodyHandlers.ofString());
        if (response.statusCode() / 100 != 2 && response.statusCode() != 409) {
            throw new IOException("S3 create bucket " + bucket + ": HTTP " + response.statusCode() + " " + abbreviate(response.body()));
        }
    }

    @Override
    public String describe() {
        return "s3:" + endpoint + "/" + bucket;
    }

    private String objectPath(String key) {
        StringBuilder sb = new StringBuilder("/").append(encode(bucket));
        for (String segment : key.split("/")) {
            sb.append('/').append(encode(segment));
        }
        return sb.toString();
    }

    private <T> HttpResponse<T> send(String method, String path, byte[] body, String contentType, HttpResponse.BodyHandler<T> handler)
            throws IOException {
        ZonedDateTime now = ZonedDateTime.now(ZoneOffset.UTC);
        String amzDate = AMZ_DATE.format(now);
        String day = AMZ_DAY.format(now);
        String payloadHash = (body == null || body.length == 0) ? EMPTY_SHA256 : hex(sha256(body));
        int port = endpoint.getPort();
        boolean defaultPort = port < 0 || ("https".equals(endpoint.getScheme()) && port == 443) || ("http".equals(endpoint.getScheme()) && port == 80);
        String host = endpoint.getHost() + (defaultPort ? "" : ":" + port);
        String basePath = (endpoint.getRawPath() != null) ? endpoint.getRawPath() : "";
        String canonicalPath = basePath + path;
        String signedHeaders = "host;x-amz-content-sha256;x-amz-date";
        String canonicalRequest = method + "\n" + canonicalPath + "\n\n"
                + "host:" + host + "\n" + "x-amz-content-sha256:" + payloadHash + "\n" + "x-amz-date:" + amzDate + "\n\n"
                + signedHeaders + "\n" + payloadHash;
        String scope = day + "/" + region + "/s3/aws4_request";
        String stringToSign = "AWS4-HMAC-SHA256\n" + amzDate + "\n" + scope + "\n" + hex(sha256(canonicalRequest.getBytes(StandardCharsets.UTF_8)));
        byte[] key = hmac(("AWS4" + secretKey).getBytes(StandardCharsets.UTF_8), day);
        key = hmac(key, region);
        key = hmac(key, "s3");
        key = hmac(key, "aws4_request");
        String signature = hex(hmac(key, stringToSign));
        HttpRequest.Builder builder = HttpRequest.newBuilder(URI.create(endpoint.getScheme() + "://" + host + canonicalPath))
                .timeout(Duration.ofSeconds(60))
                .header("x-amz-content-sha256", payloadHash)
                .header("x-amz-date", amzDate)
                .header("Authorization", "AWS4-HMAC-SHA256 Credential=" + accessKey + "/" + scope
                        + ", SignedHeaders=" + signedHeaders + ", Signature=" + signature);
        if (contentType != null) {
            builder.header("Content-Type", contentType);
        }
        builder.method(method, (body == null) ? HttpRequest.BodyPublishers.noBody() : HttpRequest.BodyPublishers.ofByteArray(body));
        try {
            return client.send(builder.build(), handler);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            throw new IOException("S3 request interrupted", e);
        }
    }

    /** RFC 3986 encoding of one path segment, as SigV4 wants it (unreserved characters stay). */
    static String encode(String segment) {
        StringBuilder sb = new StringBuilder();
        for (byte b : segment.getBytes(StandardCharsets.UTF_8)) {
            char c = (char) (b & 0xff);
            if ((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c == '-' || c == '_' || c == '.' || c == '~') {
                sb.append(c);
            } else {
                sb.append('%').append(Character.toUpperCase(Character.forDigit((b >> 4) & 0xf, 16)))
                        .append(Character.toUpperCase(Character.forDigit(b & 0xf, 16)));
            }
        }
        return sb.toString();
    }

    private static byte[] sha256(byte[] data) {
        try {
            return MessageDigest.getInstance("SHA-256").digest(data);
        } catch (NoSuchAlgorithmException e) {
            throw new IllegalStateException(e);
        }
    }

    private static byte[] hmac(byte[] key, String data) {
        try {
            Mac mac = Mac.getInstance("HmacSHA256");
            mac.init(new SecretKeySpec(key, "HmacSHA256"));
            return mac.doFinal(data.getBytes(StandardCharsets.UTF_8));
        } catch (NoSuchAlgorithmException | InvalidKeyException e) {
            throw new IllegalStateException(e);
        }
    }

    private static String hex(byte[] data) {
        StringBuilder sb = new StringBuilder(data.length * 2);
        for (byte b : data) {
            sb.append(Character.forDigit((b >> 4) & 0xf, 16)).append(Character.forDigit(b & 0xf, 16));
        }
        return sb.toString();
    }

    private static String abbreviate(String text) {
        return (text == null) ? "" : (text.length() > 300 ? text.substring(0, 300) + "..." : text);
    }
}
