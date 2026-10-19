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
package com.ilscipio.scipio.accounting.einvoice;

/**
 * E-invoice formats of the EN 16931 family.
 * <p>
 * Every format is made from one CII document (Mustang): UBL formats convert it with the EN 16931 CII to UBL 2.1
 * converter, and the hybrid PDF format embeds it in a PDF/A-3 invoice. The enum name is the format id (seed data:
 * Enumeration enumTypeId="EINV_FORMAT").
 */
public enum EInvoiceFormat {

    XRECHNUNG_UBL(Syntax.UBL, "XRECHNUNG", Ids.XRECHNUNG_3_0, Ids.PEPPOL_BILLING_PROFILE, true, false),
    XRECHNUNG_CII(Syntax.CII, "XRECHNUNG", Ids.XRECHNUNG_3_0, Ids.PEPPOL_BILLING_PROFILE, true, false),
    ZUGFERD(Syntax.PDF, "EN16931", Ids.EN16931, null, false, false),
    PEPPOL_BIS3(Syntax.UBL, "EN16931", Ids.PEPPOL_BIS_3_0, Ids.PEPPOL_BILLING_PROFILE, false, true),
    EN16931_UBL(Syntax.UBL, "EN16931", Ids.EN16931, Ids.PEPPOL_BILLING_PROFILE, false, false),
    EN16931_CII(Syntax.CII, "EN16931", Ids.EN16931, null, false, false);

    public enum Syntax { CII, UBL, PDF }

    /** Specification identifiers (BT-24) and the business process (BT-23). */
    public static final class Ids {
        public static final String EN16931 = "urn:cen.eu:en16931:2017";
        public static final String XRECHNUNG_3_0 = "urn:cen.eu:en16931:2017#compliant#urn:xeinkauf.de:kosit:xrechnung_3.0";
        public static final String PEPPOL_BIS_3_0 = "urn:cen.eu:en16931:2017#compliant#urn:fdc:peppol.eu:2017:poacc:billing:3.0";
        public static final String PEPPOL_BILLING_PROFILE = "urn:fdc:peppol.eu:2017:poacc:billing:01:1.0";
        private Ids() {}
    }

    private final Syntax syntax;
    private final String ciiProfile;
    private final String customizationId;
    private final String profileId;
    private final boolean xrechnung;
    private final boolean peppol;

    EInvoiceFormat(Syntax syntax, String ciiProfile, String customizationId, String profileId, boolean xrechnung, boolean peppol) {
        this.syntax = syntax;
        this.ciiProfile = ciiProfile;
        this.customizationId = customizationId;
        this.profileId = profileId;
        this.xrechnung = xrechnung;
        this.peppol = peppol;
    }

    /** Returns the format for the id (the enum name, case-insensitive), or null. */
    public static EInvoiceFormat fromId(String formatId) {
        if (formatId == null || formatId.isEmpty()) {
            return null;
        }
        for (EInvoiceFormat format : values()) {
            if (format.name().equalsIgnoreCase(formatId.trim())) {
                return format;
            }
        }
        return null;
    }

    public String getId() { return name(); }

    public Syntax getSyntax() { return syntax; }

    /** Mustang profile name of the CII document (EN16931, XRECHNUNG). */
    public String getCiiProfile() { return ciiProfile; }

    /** Specification identifier (BT-24). */
    public String getCustomizationId() { return customizationId; }

    /** Business process type (BT-23), or null. */
    public String getProfileId() { return profileId; }

    /** True if the German XRechnung rules (BR-DE-*) apply. */
    public boolean isXRechnung() { return xrechnung; }

    /** True if the Peppol BIS rules (PEPPOL-EN16931-*) apply. */
    public boolean isPeppol() { return peppol; }

    public boolean isPdf() { return syntax == Syntax.PDF; }

    public String getContentType() {
        return isPdf() ? "application/pdf" : "application/xml";
    }

    public String getFileName(String invoiceNumber) {
        String base = (invoiceNumber != null ? invoiceNumber : "invoice").replaceAll("[^A-Za-z0-9._-]", "_");
        switch (this) {
        case ZUGFERD: return base + ".pdf";
        case XRECHNUNG_UBL: return base + "_xrechnung-ubl.xml";
        case XRECHNUNG_CII: return base + "_xrechnung-cii.xml";
        case PEPPOL_BIS3: return base + "_peppol.xml";
        case EN16931_UBL: return base + "_ubl.xml";
        default: return base + "_cii.xml";
        }
    }
}
