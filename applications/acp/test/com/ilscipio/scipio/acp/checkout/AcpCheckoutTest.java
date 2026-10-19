package com.ilscipio.scipio.acp.checkout;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.Test;

import com.ilscipio.scipio.acp.AcpException;

public class AcpCheckoutTest {

    private static Map<String, Object> item(Object id, Object qty) {
        Map<String, Object> m = new LinkedHashMap<>();
        if (id != null) m.put("id", id);
        if (qty != null) m.put("quantity", qty);
        return m;
    }

    private static List<Object> items(Object... maps) {
        return new ArrayList<>(Arrays.asList(maps));
    }

    private static AcpException fail(Object... maps) {
        return assertThrows(AcpException.class, () -> AcpCheckout.normalizeItems(items(maps), "$.line_items"));
    }

    @Test
    void mergesRepeatedIdsAndDefaultsQuantity() throws Exception {
        List<Map<String, Object>> out = AcpCheckout.normalizeItems(
                items(item("A", 2), item("B", null), item("A", "3")), "$.line_items");
        assertEquals(2, out.size());
        assertEquals("A", out.get(0).get("id"));
        assertEquals(5L, out.get(0).get("quantity"));
        assertEquals("B", out.get(1).get("id"));
        assertEquals(1L, out.get(1).get("quantity"));
    }

    @Test
    void acceptsBounds() throws Exception {
        assertEquals(10000L, AcpCheckout.normalizeItems(items(item("A", 10000)), "$.x").get(0).get("quantity"));
        assertEquals(1L, AcpCheckout.normalizeItems(items(item("A", 1)), "$.x").get(0).get("quantity"));
    }

    @Test
    void rejectsQuantityZero() {
        AcpException e = fail(item("A", 1), item("B", 0));
        assertEquals(400, e.getStatus());
        assertEquals("$.line_items[1].quantity", e.getParam());
    }

    @Test
    void rejectsQuantityTooBig() {
        AcpException e = fail(item("A", 10001));
        assertEquals(400, e.getStatus());
        assertEquals("$.line_items[0].quantity", e.getParam());
    }

    @Test
    void rejectsFractionAndText() {
        assertEquals("$.line_items[0].quantity", fail(item("A", 1.5)).getParam());
        assertEquals("$.line_items[0].quantity", fail(item("A", "many")).getParam());
    }

    @Test
    void rejectsMissingId() {
        AcpException e = fail(item("A", 1), item(null, 1));
        assertEquals(400, e.getStatus());
        assertEquals("missing", e.getCode());
        assertEquals("$.line_items[1].id", e.getParam());
    }

    @Test
    void rejectsNonObjectItem() {
        assertEquals("$.line_items[0].id", fail("just-a-string").getParam());
    }

    @Test
    void rejectsMoreThan100Items() {
        List<Object> many = new ArrayList<>();
        for (int i = 0; i < 101; i++) many.add(item("P" + i, 1));
        AcpException e = assertThrows(AcpException.class, () -> AcpCheckout.normalizeItems(many, "$.line_items"));
        assertEquals("$.line_items", e.getParam());
    }

    @Test
    void discountCodesMergeAndDedup() {
        Map<String, Object> body = new LinkedHashMap<>();
        Map<String, Object> d = new LinkedHashMap<>();
        d.put("codes", Arrays.asList("SAVE10", " WELCOME ", "SAVE10"));
        body.put("discounts", d);
        body.put("coupons", Arrays.asList("WELCOME", "OLD"));
        assertEquals(Arrays.asList("SAVE10", "WELCOME", "OLD"), AcpCheckout.discountCodes(body));
        assertEquals(0, AcpCheckout.discountCodes(new LinkedHashMap<>()).size());
    }

    @Test
    void sharedPaymentTokenCurrentAndLegacy() {
        Map<String, Object> cred = new LinkedHashMap<>();
        cred.put("type", "spt");
        cred.put("token", " spt_123 ");
        Map<String, Object> instrument = new LinkedHashMap<>();
        instrument.put("credential", cred);
        Map<String, Object> pd = new LinkedHashMap<>();
        pd.put("instrument", instrument);
        assertEquals("spt_123", AcpCheckout.sharedPaymentToken(pd));

        Map<String, Object> legacy = new LinkedHashMap<>();
        legacy.put("token", "spt_old");
        legacy.put("provider", "stripe");
        assertEquals("spt_old", AcpCheckout.sharedPaymentToken(legacy));

        assertNull(AcpCheckout.sharedPaymentToken(new LinkedHashMap<>()));
    }
}
