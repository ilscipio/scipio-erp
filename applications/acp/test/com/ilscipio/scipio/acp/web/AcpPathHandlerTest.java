package com.ilscipio.scipio.acp.web;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.time.Instant;
import java.util.Base64;

import org.junit.jupiter.api.Test;

import com.ilscipio.scipio.acp.AcpException;
import com.ilscipio.scipio.acp.AcpJson;

public class AcpPathHandlerTest {

    private static final Instant NOW = Instant.parse("2026-10-19T12:00:00Z");
    private static final String TS = "2026-10-19T12:00:00Z";
    private static final String BODY = "{\"a\":1}";

    private static String sign(String secret, String ts, String body) {
        return Base64.getEncoder().encodeToString(AcpJson.hmacSha256(secret, ts + "." + body));
    }

    private static void verify(String secret, String sig, String ts) throws AcpException {
        AcpPathHandler.verifySignature(secret, sig, ts, BODY, 300, NOW);
    }

    private static void assert401(AcpException e) {
        assertEquals(401, e.getStatus());
        assertEquals("invalid_signature", e.getCode());
    }

    @Test
    void validSignaturePasses() throws Exception {
        verify("s3cret", sign("s3cret", TS, BODY), TS);
    }

    @Test
    void v1PrefixAccepted() throws Exception {
        verify("s3cret", "v1=" + sign("s3cret", TS, BODY), TS);
    }

    @Test
    void urlSafeUnpaddedAccepted() throws Exception {
        String s = sign("s3cret", TS, BODY).replace('+', '-').replace('/', '_').replace("=", "");
        verify("s3cret", s, TS);
    }

    @Test
    void wrongSecretFails() {
        assert401(assertThrows(AcpException.class, () -> verify("s3cret", sign("other", TS, BODY), TS)));
    }

    @Test
    void tamperedBodyFails() {
        assert401(assertThrows(AcpException.class,
                () -> AcpPathHandler.verifySignature("s3cret", sign("s3cret", TS, BODY), TS, "{\"a\":2}", 300, NOW)));
    }

    @Test
    void missingHeadersFail() {
        assert401(assertThrows(AcpException.class, () -> verify("s3cret", null, TS)));
        assert401(assertThrows(AcpException.class, () -> verify("s3cret", "", TS)));
        assert401(assertThrows(AcpException.class, () -> verify("s3cret", sign("s3cret", TS, BODY), null)));
    }

    @Test
    void badTimestampFails() {
        assert401(assertThrows(AcpException.class, () -> verify("s3cret", sign("s3cret", "yesterday", BODY), "yesterday")));
    }

    @Test
    void tooOldFails() {
        String old = "2026-10-19T11:00:00Z";
        assert401(assertThrows(AcpException.class, () -> verify("s3cret", sign("s3cret", old, BODY), old)));
    }

    @Test
    void tooFarInFutureFails() {
        String future = "2026-10-19T13:00:00Z";
        assert401(assertThrows(AcpException.class, () -> verify("s3cret", sign("s3cret", future, BODY), future)));
    }
}
