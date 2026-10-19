package com.ilscipio.scipio.acp;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;

import java.util.Map;

import org.junit.jupiter.api.Test;

public class AcpExceptionTest {

    @Test
    void bodyWithoutParam() {
        Map<String, Object> b = AcpException.notFound("gone").toBody();
        assertEquals("invalid_request", b.get("type"));
        assertEquals("not_found", b.get("code"));
        assertEquals("gone", b.get("message"));
        assertFalse(b.containsKey("param"));
    }

    @Test
    void bodyWithParam() {
        AcpException e = AcpException.invalid("missing", "need it", "$.x");
        assertEquals(400, e.getStatus());
        assertEquals("$.x", e.toBody().get("param"));
    }
}
