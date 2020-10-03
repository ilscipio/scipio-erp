package org.ofbiz.entity.util;

import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;

import java.sql.Timestamp;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.List;

/**
 * Helper class to pass around filterByDate information to {@link EntityQuery} (SCIPIO).
 * NOTE: Factory methods duplicate the behavior of {@link EntityQuery#filterByDate(Timestamp, String...)} (even where ugly) to avoid inconsistency.
 */
public class QueryDateFilter {
    public static final QueryDateFilter DEFAULT = new QueryDateFilter(true, null, UtilMisc.unmodifiableArrayList("fromDate", "thruDate"));
    public static final QueryDateFilter DISABLED = new QueryDateFilter(false, null, Collections.emptyList());

    protected final boolean enabled;
    protected final Timestamp moment;
    protected final List<String> fieldNames;

    private QueryDateFilter(boolean enabled, Timestamp moment, List<String> fieldNames) {
        this.enabled = enabled;
        this.moment = moment;
        this.fieldNames = UtilValidate.isNotEmpty(fieldNames) ? fieldNames : null;
    }

    public static QueryDateFilter from(boolean enabled, Timestamp moment, List<String> fieldNames) {
        return enabled ? new QueryDateFilter(true, moment, fieldNames) : DISABLED;
    }

    public static QueryDateFilter from(boolean enabled, Timestamp moment, String... fieldNames) {
        return enabled ? new QueryDateFilter(true, moment, Arrays.asList(fieldNames)) : DISABLED;
    }

    public static QueryDateFilter from(boolean enabled, Timestamp moment) {
        return enabled ? new QueryDateFilter(true, moment, null) : DISABLED;
    }

    public static QueryDateFilter from(boolean enabled, Date moment) {
        return enabled ? new QueryDateFilter(true, new java.sql.Timestamp(moment.getTime()), null) : DISABLED;
    }

    public static QueryDateFilter from(Timestamp moment, List<String> fieldNames) {
        return new QueryDateFilter(true, moment, fieldNames);
    }

    public static QueryDateFilter from(Timestamp moment, String... fieldNames) {
        return new QueryDateFilter(true, moment, Arrays.asList(fieldNames));
    }

    public static QueryDateFilter from(Timestamp moment) {
        return (moment != null) ? new QueryDateFilter(true, moment, DEFAULT.getFieldNames()) : DISABLED;
    }

    public static QueryDateFilter from(Date moment) {
        return (moment != null) ? new QueryDateFilter(true, new java.sql.Timestamp(moment.getTime()), DEFAULT.getFieldNames()) : DISABLED;
    }

    public static QueryDateFilter from(List<String> fieldNames) {
        return new QueryDateFilter(true, null, fieldNames);
    }

    public static QueryDateFilter from(String... fieldNames) {
        return new QueryDateFilter(true, null, Arrays.asList(fieldNames));
    }

    public boolean isEnabled() {
        return enabled;
    }

    /** Returns moment; may be null even if enabled to delay UtilDateTime.nowTimestamp to moment of query. */
    public Timestamp getMoment() {
        return moment;
    }

    public List<String> getFieldNames() {
        return fieldNames;
    }
}
