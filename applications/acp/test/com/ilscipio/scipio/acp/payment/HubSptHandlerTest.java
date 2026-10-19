package com.ilscipio.scipio.acp.payment;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.Map;

import org.junit.jupiter.api.Test;

import com.ilscipio.scipio.acp.AcpJson;
import com.ilscipio.scipio.acp.payment.AcpPaymentHandler.ChargeRequest;
import com.ilscipio.scipio.acp.payment.AcpPaymentHandler.ChargeResult;

public class HubSptHandlerTest {

    private static Map<String, Object> json(String s) throws Exception {
        return AcpJson.parse(s);
    }

    @Test
    void succeededAndProcessing() throws Exception {
        ChargeResult r = HubSptHandler.result(200, json("{\"status\":\"succeeded\",\"payment_intent\":\"pi_9\"}"));
        assertEquals(ChargeResult.Status.SUCCEEDED, r.status);
        assertEquals("pi_9", r.reference);
        assertEquals(ChargeResult.Status.SUCCEEDED, HubSptHandler.result(200, json("{\"status\":\"processing\"}")).status);
    }

    @Test
    void requiresAction() throws Exception {
        ChargeResult r = HubSptHandler.result(200, json("{\"status\":\"requires_action\",\"payment_intent\":\"pi_8\"}"));
        assertEquals(ChargeResult.Status.ACTION_REQUIRED, r.status);
        assertEquals("requires_3ds", r.code);
    }

    @Test
    void declinedWithFailureCode() throws Exception {
        ChargeResult r = HubSptHandler.result(402, json("{\"status\":\"failed\",\"failure_code\":\"card_declined\",\"message\":\"x\"}"));
        assertEquals(ChargeResult.Status.DECLINED, r.status);
        assertEquals("card_declined", r.code);
        assertEquals("x", r.message);
    }

    @Test
    void declinedDefaultCode() throws Exception {
        ChargeResult r = HubSptHandler.result(200, json("{\"status\":\"canceled\"}"));
        assertEquals(ChargeResult.Status.DECLINED, r.status);
        assertEquals("payment_declined", r.code);
    }

    @Test
    void httpErrorWithoutStatus() throws Exception {
        ChargeResult r = HubSptHandler.result(403, json("{\"message\":\"bad token\"}"));
        assertEquals(ChargeResult.Status.DECLINED, r.status);
        assertEquals("hub_refused", r.code);
        assertEquals("bad token", r.message);
        assertEquals("fc", HubSptHandler.result(400, json("{\"failure_code\":\"fc\"}")).code);
    }

    @Test
    void claims() {
        ChargeRequest req = new ChargeRequest();
        req.checkoutSessionId = "cs_1";
        req.orderId = "10000";
        req.amountMinor = 1999;
        req.currency = "USD";
        req.sharedPaymentToken = "spt_abc";
        req.buyerEmail = "a@b.c";
        Map<String, Object> c = HubSptHandler.claims("store1", req, "https://shop.example", 1000L);
        assertEquals("acp_spt", c.get("kind"));
        assertEquals("cs_1", c.get("session"));
        assertEquals(AcpJson.sha256Hex("spt_abc"), c.get("spt"));
        assertEquals("19.99", c.get("amount"));
        assertEquals("USD", c.get("currency"));
        assertEquals("store1", c.get("store"));
        assertEquals("10000", c.get("orderId"));
    }
}
