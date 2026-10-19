package com.ilscipio.scipio.acp.web;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.Test;

import com.ilscipio.scipio.acp.AcpJson;

public class AcpMcpBindingTest {

    @Test
    void fiveTools() {
        List<String> names = new ArrayList<>();
        for (Map<String, Object> t : AcpMcpBinding.tools()) {
            names.add((String) t.get("name"));
            Map<String, Object> schema = AcpJson.map(t.get("inputSchema"));
            assertEquals("object", schema.get("type"));
            assertTrue(AcpJson.list(schema.get("required")).contains("meta"), t.get("name") + " requires meta");
        }
        assertEquals(Arrays.asList("create_checkout_session", "get_checkout_session", "update_checkout_session",
                "complete_checkout_session", "cancel_checkout_session"), names);
    }

    @Test
    void idRequiredExceptForCreate() {
        for (Map<String, Object> t : AcpMcpBinding.tools()) {
            List<Object> req = AcpJson.list(AcpJson.map(t.get("inputSchema")).get("required"));
            assertEquals(!"create_checkout_session".equals(t.get("name")), req.contains("id"), String.valueOf(t.get("name")));
        }
    }

    @Test
    void resultEnvelope() {
        Map<String, Object> m = AcpMcpBinding.result(7, Map.of("ok", true));
        assertEquals("2.0", m.get("jsonrpc"));
        assertEquals(7, m.get("id"));
        assertEquals(Map.of("ok", true), m.get("result"));
        assertFalse(m.containsKey("error"));
    }

    @Test
    void errorEnvelope() {
        Map<String, Object> m = AcpMcpBinding.error("abc", AcpMcpBinding.ACP_ERROR, "boom", Map.of("code", "x"));
        assertEquals("2.0", m.get("jsonrpc"));
        assertEquals("abc", m.get("id"));
        assertFalse(m.containsKey("result"));
        Map<String, Object> e = AcpJson.map(m.get("error"));
        assertEquals(-32000, e.get("code"));
        assertEquals("boom", e.get("message"));
        assertEquals(Map.of("code", "x"), e.get("data"));
        assertFalse(AcpJson.map(AcpMcpBinding.error(1, -32602, "m", null).get("error")).containsKey("data"));
    }
}
