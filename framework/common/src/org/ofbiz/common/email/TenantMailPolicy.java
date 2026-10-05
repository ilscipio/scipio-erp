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
package org.ofbiz.common.email;

import java.time.LocalDate;
import java.time.ZoneOffset;
import java.util.HashSet;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;

import javax.mail.internet.AddressException;
import javax.mail.internet.InternetAddress;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.StringUtil;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.DelegatorFactory;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.entity.util.Tenants;

/**
 * SCIPIO: 4.0.0: Pooled runtime: mail rules per store (G9), applied by sendMail when the delegator belongs to a store.
 *
 * <ul>
 * <li>Sender domain: the From address must use a domain of the store (a host of the store in the master table
 * {@code TenantDomainName}, or that host without "www.") or a platform domain ({@code tenant.mail.platformDomains}). The
 * store cannot widen this list: it comes from the master tables and general.properties, never from the store's own
 * SystemProperty rows. Another From address is replaced by {@code tenant.mail.platformSender} (the original address
 * becomes Reply-To), or the mail is refused when no platform sender is set.</li>
 * <li>Quota: at most the plan's mailPerDay mails per store and day (UTC) in this JVM. The relay enforces the quota
 * across JVMs; this is the first guard.</li>
 * </ul>
 */
public final class TenantMailPolicy {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    private static final long DOMAIN_CACHE_TTL_MS = 60000L;

    private static final Map<String, AtomicInteger> sentToday = new ConcurrentHashMap<>();
    private static volatile LocalDate day = LocalDate.now(ZoneOffset.UTC);
    private static final Map<String, DomainEntry> domainCache = new ConcurrentHashMap<>();

    private TenantMailPolicy() {}

    private static final class DomainEntry {
        final Set<String> domains;
        final long expires;

        DomainEntry(Set<String> domains, long expires) {
            this.domains = domains;
            this.expires = expires;
        }
    }

    /** The result of a check: an error, or the From address (and an extra Reply-To) to use. */
    public static final class Decision {
        private final String error;
        private final String sendFrom;
        private final String replyTo;

        Decision(String error, String sendFrom, String replyTo) {
            this.error = error;
            this.sendFrom = sendFrom;
            this.replyTo = replyTo;
        }

        public String getError() { return error; }
        public String getSendFrom() { return sendFrom; }
        /** The original From address when it was replaced; null otherwise. */
        public String getReplyTo() { return replyTo; }
    }

    /** True when the rules apply: pooled runtime and a store delegator. */
    public static boolean applies(Delegator delegator) {
        return Tenants.isPooled() && delegator != null && delegator.getDelegatorTenantId() != null;
    }

    /** Checks the sender domain and whether the store has quota left today (takes none: see {@link #takeQuota}). */
    public static Decision check(Delegator delegator, String sendFrom) {
        String tenantId = delegator.getDelegatorTenantId();
        String replyTo = null;
        String from = sendFrom;
        if (UtilProperties.getPropertyAsBoolean("general", "tenant.mail.senderDomainCheck", true) && !isAllowedSender(tenantId, sendFrom)) {
            String platformSender = UtilProperties.getPropertyValue("general", "tenant.mail.platformSender");
            if (UtilValidate.isEmpty(platformSender)) {
                Debug.logWarning("Mail: store [" + tenantId + "] may not send from [" + sendFrom + "]; mail refused", module);
                return new Decision("The sender address " + sendFrom + " does not belong to this store", null, null);
            }
            from = platformSender.replace("${tenantId}", tenantId);
            replyTo = sendFrom;
            Debug.logInfo("Mail: store [" + tenantId + "] sender [" + sendFrom + "] replaced by [" + from + "]", module);
        }
        int limit = Tenants.getPlan(tenantId).getMailPerDay();
        if (limit > 0 && counter(tenantId).get() >= limit) {
            Debug.logWarning("Mail: store [" + tenantId + "] reached its quota of " + limit + " mails per day; mail refused", module);
            return new Decision("The store reached its mail quota of " + limit + " mails per day", null, null);
        }
        return new Decision(null, from, replyTo);
    }

    /**
     * Takes one unit of the store's daily quota just before the mail goes to the relay; returns an error when the quota
     * is used up (another thread took the last unit after {@link #check}). A mail that is not sent (mail disabled,
     * build error) takes no unit.
     */
    public static String takeQuota(Delegator delegator) {
        String tenantId = delegator.getDelegatorTenantId();
        int limit = Tenants.getPlan(tenantId).getMailPerDay();
        if (limit > 0 && counter(tenantId).incrementAndGet() > limit) {
            Debug.logWarning("Mail: store [" + tenantId + "] reached its quota of " + limit + " mails per day; mail refused", module);
            return "The store reached its mail quota of " + limit + " mails per day";
        }
        return null;
    }

    private static AtomicInteger counter(String tenantId) {
        LocalDate today = LocalDate.now(ZoneOffset.UTC);
        if (!today.equals(day)) {
            synchronized (TenantMailPolicy.class) {
                if (!today.equals(day)) {
                    sentToday.clear();
                    day = today;
                }
            }
        }
        return sentToday.computeIfAbsent(tenantId, k -> new AtomicInteger());
    }

    /** True when the address uses a domain of the store or a platform domain. */
    public static boolean isAllowedSender(String tenantId, String sendFrom) {
        if (UtilValidate.isEmpty(sendFrom)) {
            return false;
        }
        String domain;
        try {
            String address = new InternetAddress(sendFrom, true).getAddress();
            int at = address.lastIndexOf('@');
            if (at < 0) {
                return false;
            }
            domain = address.substring(at + 1).toLowerCase(Locale.ROOT);
        } catch (AddressException e) {
            return false;
        }
        // StringUtil.split returns null for an empty value: no platform domains configured.
        java.util.List<String> platformDomains = StringUtil.split(UtilProperties.getPropertyValue("general", "tenant.mail.platformDomains", ""), ",");
        if (platformDomains != null) {
            for (String allowed : platformDomains) {
                if (domain.equals(allowed.trim().toLowerCase(Locale.ROOT))) {
                    return true;
                }
            }
        }
        return getStoreDomains(tenantId).contains(domain);
    }

    /** The store's hosts from TenantDomainName and their parent domains (shop.example.com -> example.com). */
    private static Set<String> getStoreDomains(String tenantId) {
        long now = System.currentTimeMillis();
        DomainEntry entry = domainCache.get(tenantId);
        if (entry != null && entry.expires > now) {
            return entry.domains;
        }
        Set<String> domains = new HashSet<>();
        try {
            Delegator base = DelegatorFactory.getDelegator(Tenants.BASE_DELEGATOR);
            for (GenericValue row : EntityQuery.use(base).from("TenantDomainName").where("tenantId", tenantId).queryList()) {
                String host = row.getString("domainName").toLowerCase(Locale.ROOT);
                domains.add(host);
                // www.example.com -> example.com; no other parent domain (brand.co.uk must not allow co.uk)
                if (host.startsWith("www.") && host.indexOf('.', 4) > 0) {
                    domains.add(host.substring(4));
                }
            }
        } catch (GenericEntityException e) {
            Debug.logError(e, "Mail: could not read the domains of store [" + tenantId + "]", module);
        }
        domainCache.put(tenantId, new DomainEntry(domains, now + DOMAIN_CACHE_TTL_MS));
        return domains;
    }
}
