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
package com.ilscipio.scipio.countrypack.core;

import java.util.Collections;
import java.util.List;
import java.util.Map;

/**
 * One country pack, read from {@code <id>/pack.json} (blueprint section 6). A pack is data: the framework reads it and
 * never runs code from it. Every part of the table in section 6 has a field here.
 *
 * <p>The fields are public because Jackson fills them (the build uses Java 11: no records). Code reads a pack through the
 * accessor methods, which never return null for a list or a map.</p>
 *
 * <p>SCIPIO: 4.0.0: Added (W1-03).</p>
 */
public final class Pack {
    public String id;
    public int version;
    public String name;
    public String country;
    public String locale;
    public String currency;
    public String dataRegion;
    public List<String> jurisdictions;
    public Map<String, String> profile;
    public List<SellerField> sellerFields;
    public List<Template> legalTemplates;
    public List<Task> tasks;
    public List<String> storefrontRules;
    public Tax tax;
    public List<String> invoiceNotes;
    public List<String> payments;
    public List<String> carriers;
    public String books;
    public List<String> verify;

    public String id() {
        return id;
    }

    public int version() {
        return version;
    }

    public String name() {
        return name;
    }

    public String country() {
        return country;
    }

    public String locale() {
        return locale;
    }

    public String currency() {
        return currency;
    }

    public String dataRegion() {
        return dataRegion;
    }

    public List<String> jurisdictions() {
        return nn(jurisdictions);
    }

    public Map<String, String> profile() {
        return profile == null ? Collections.<String, String>emptyMap() : profile;
    }

    public List<SellerField> sellerFields() {
        return nn(sellerFields);
    }

    public List<Template> legalTemplates() {
        return nn(legalTemplates);
    }

    public List<Task> tasks() {
        return nn(tasks);
    }

    public List<String> storefrontRules() {
        return nn(storefrontRules);
    }

    public Tax tax() {
        return tax;
    }

    public List<String> invoiceNotes() {
        return nn(invoiceNotes);
    }

    public List<String> payments() {
        return nn(payments);
    }

    public List<String> carriers() {
        return nn(carriers);
    }

    public String books() {
        return books;
    }

    public List<String> verify() {
        return nn(verify);
    }

    private static <T> List<T> nn(List<T> l) {
        return l == null ? Collections.<T>emptyList() : l;
    }

    /** True when a task or a text with this scope (any, home or market) applies to a pack in this role. */
    public boolean appliesTo(String scope, Role role) {
        return scope == null || "any".equals(scope) || role.scope().equals(scope);
    }

    /** A field that the seller fills in (legal form, register number ...). {@code target} names the place: an entity field. */
    public static final class SellerField {
        public String id;
        public String label;
        public boolean required;
        public String target;
    }

    /**
     * A legal text. {@code scope}: home, market or any. {@code sinceVersion}: the pack version that added or changed the text;
     * a store on an older version gets a review task instead of a silent change.
     */
    public static final class Template {
        public String slug;
        public String docTypeId;
        public String scope;
        public int sinceVersion;

        public String slug() {
            return slug;
        }

        public String docTypeId() {
            return docTypeId;
        }

        public String scope() {
            return scope == null ? "any" : scope;
        }

        public int sinceVersion() {
            return sinceVersion <= 0 ? 1 : sinceVersion;
        }
    }

    /**
     * A setup task. {@code kind}: registration, decision or info. {@code numberLabel}: the task asks for a number.
     * {@code eprScheme} and {@code eprCountry} (for example EPR_PACKAGING, DEU): the number is also saved as an EPR registration
     * of the store owner, so the imprint shows it and the compliance checklist sees it.
     */
    public static final class Task {
        public String id;
        public String title;
        public String kind;
        public boolean required;
        public String scope;
        public String link;
        public String numberLabel;
        public String detail;
        public int sinceVersion;
        public String eprScheme;
        public String eprCountry;

        public String id() {
            return id;
        }

        public String title() {
            return title;
        }

        public String kind() {
            return kind == null ? "info" : kind;
        }

        public boolean required() {
            return required;
        }

        public String scope() {
            return scope == null ? "any" : scope;
        }

        public String link() {
            return link;
        }

        public String numberLabel() {
            return numberLabel;
        }

        public String detail() {
            return detail;
        }

        public int sinceVersion() {
            return sinceVersion <= 0 ? 1 : sinceVersion;
        }

        public String eprScheme() {
            return eprScheme;
        }

        public String eprCountry() {
            return eprCountry;
        }
    }

    public static final class Tax {
        public String provider;
        public Integer ossThresholdEur;
        public List<TaxNote> notes;

        public List<TaxNote> notes() {
            return nn(notes);
        }
    }

    public static final class TaxNote {
        public String id;
        public String text;
    }
}
