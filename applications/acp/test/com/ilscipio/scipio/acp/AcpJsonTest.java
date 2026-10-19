package com.ilscipio.scipio.acp;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.math.BigDecimal;

import org.junit.jupiter.api.Test;

public class AcpJsonTest {

    @Test
    void toMinorRoundsHalfUp() {
        assertEquals(1235L, AcpJson.toMinor(new BigDecimal("12.345"), "USD"));
        assertEquals(1234L, AcpJson.toMinor(new BigDecimal("12.344"), "USD"));
        assertEquals(0L, AcpJson.toMinor(null, "USD"));
    }

    @Test
    void currencyDigits() {
        assertEquals(500L, AcpJson.toMinor(new BigDecimal("500"), "JPY"));
        assertEquals(12345L, AcpJson.toMinor(new BigDecimal("12.345"), "KWD"));
        assertEquals(0, AcpJson.minorDigits("JPY"));
        assertEquals(3, AcpJson.minorDigits("kwd"));
        assertEquals(2, AcpJson.minorDigits("XXXX"));
        assertEquals(2, AcpJson.minorDigits(null));
    }

    @Test
    void fromMinor() {
        assertEquals(new BigDecimal("19.99"), AcpJson.fromMinor(1999, "USD"));
        assertEquals(0, new BigDecimal("500").compareTo(AcpJson.fromMinor(500, "JPY")));
        assertEquals(new BigDecimal("12.345"), AcpJson.fromMinor(12345, "KWD"));
    }

    @Test
    void sha256() {
        assertEquals("e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855", AcpJson.sha256Hex(""));
        assertEquals("ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad", AcpJson.sha256Hex("abc"));
    }

    @Test
    void hmac() {
        byte[] mac = AcpJson.hmacSha256("key", "The quick brown fox jumps over the lazy dog");
        assertEquals("f7bc83f430538424b13298e6aa6fb143ef4d59a14946175997479dbc2d1a3cd8", AcpJson.hex(mac));
        assertEquals("0a0aff", AcpJson.hex(new byte[] {10, 10, (byte) 255}));
        assertEquals(32, AcpJson.hmacSha256("k", "x").length);
    }

    @Test
    void constantTimeEquals() {
        assertTrue(AcpJson.constantTimeEquals("abc", "abc"));
        assertFalse(AcpJson.constantTimeEquals("abc", "abd"));
        assertFalse(AcpJson.constantTimeEquals("abc", "abcd"));
        assertFalse(AcpJson.constantTimeEquals(null, "abc"));
        assertFalse(AcpJson.constantTimeEquals("abc", null));
    }
}
