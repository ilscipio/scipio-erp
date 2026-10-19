package com.ilscipio.scipio.acp.webhook;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import com.ilscipio.scipio.acp.AcpJson;

public class AcpWebhooksTest {

    @Test
    void signatureFormat() {
        String body = "{\"type\":\"order_create\"}";
        String expected = "t=1760875200,v1=" + AcpJson.hex(AcpJson.hmacSha256("whsec", "1760875200." + body));
        assertEquals(expected, AcpWebhooks.signature("whsec", 1760875200L, body));
    }

    @Test
    void backoffDoublesAndCaps() {
        assertEquals(60_000L, AcpWebhooks.backoffMillis(1));
        assertEquals(120_000L, AcpWebhooks.backoffMillis(2));
        assertEquals(240_000L, AcpWebhooks.backoffMillis(3));
        assertEquals(360L * 60_000L, AcpWebhooks.backoffMillis(10));
        assertEquals(360L * 60_000L, AcpWebhooks.backoffMillis(50));
    }
}
