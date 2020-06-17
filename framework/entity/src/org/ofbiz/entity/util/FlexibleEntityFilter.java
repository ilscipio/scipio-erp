package org.ofbiz.entity.util;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.string.FlexibleStringExpander;
import org.ofbiz.entity.GenericEntity;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;

/** SCIPIO: FlexibleStringExpander-based EntityMatcher with support for common and per-call contexts and entity key. */
public class FlexibleEntityFilter implements EntityFilter, Serializable {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    protected final FlexibleStringExpander matchesExpr;
    protected final Map<String, Object> commonContext;
    protected final String entityKey;

    protected FlexibleEntityFilter(FlexibleStringExpander matchesExpr, Map<String, Object> commonContext, String entityKey) {
        this.matchesExpr = matchesExpr;
        this.commonContext = UtilValidate.isNotEmpty(commonContext) ? commonContext : null;
        this.entityKey = UtilValidate.isNotEmpty(entityKey) ? entityKey : null;
    }

    public static EntityFilter fromExpr(FlexibleStringExpander matchesExpr, Map<String, Object> commonContext, String entityKey) {
        EntityFilter filter = EntityFilter.checkAnyNoneFromExprOrNull(matchesExpr != null && !matchesExpr.isEmpty() ? matchesExpr.getOriginal() : null);
        if (filter != null) {
            return filter;
        }
        return new FlexibleEntityFilter(matchesExpr, commonContext, entityKey);
    }

    public static EntityFilter fromExpr(String matchesExpr, Map<String, Object> commonContext, String entityKey) {
        EntityFilter filter = EntityFilter.checkAnyNoneFromExprOrNull(matchesExpr);
        if (filter != null) {
            return filter;
        }
        return new FlexibleEntityFilter(FlexibleStringExpander.getInstance(matchesExpr), commonContext, entityKey);
    }

    public FlexibleStringExpander getMatchesExpr() {
        return matchesExpr;
    }

    public String getOriginalExpr() { return getMatchesExpr().getOriginal(); }

    public Map<String, Object> getCommonContext() {
        return commonContext;
    }

    public String getEntityKey() {
        return entityKey;
    }

    public Map<String, Object> prepareContext(GenericEntity entity, Map<String, Object> callContext) {
        return prepareContext(entity, callContext, getCommonContext(), getEntityKey());
    }

    public static Map<String, Object> prepareContext(GenericEntity entity, Map<String, Object> callContext, Map<String, Object> commonContext, String entityKey) {
        if (commonContext == null) {
            if (entityKey != null) {
                Map<String, Object> context = new HashMap<>(callContext);
                context.put(entityKey, entity);
                return context;
            } else {
                return callContext;
            }
        } else if (callContext.isEmpty()) {
            if (entityKey != null) {
                Map<String, Object> context = new HashMap<>(commonContext);
                context.put(entityKey, entity);
                return context;
            } else {
                return commonContext;
            }
        } else {
            Map<String, Object> context = new HashMap<>(commonContext);
            context.putAll(callContext);
            if (entityKey != null) {
                context = new HashMap<>(commonContext);
                context.put(entityKey, entity);
            }
            return context;
        }
    }

    @Override
    public boolean matches(GenericEntity entity, Map<String, Object> context) {
        Object result = getMatchesExpr().expand(prepareContext(entity, context));
        if (result instanceof Boolean) {
            return (Boolean) result;
        } else if (result instanceof String) {
            Boolean booleanResult = UtilMisc.booleanValueVersatile((String) result);
            if (booleanResult != null) {
                return booleanResult;
            }
        }
        // TODO: REVIEW: I've made this log error instead of throwing expression by default for now because admin errors likely, but it's not really correct
        //  and may be undesirable...
        Debug.logError("Matches expression evaluated to non-boolean value [" + result + "] for expression ["
                + getMatchesExpr() + "] with context " + context + "; returning false", module);
        return false;
    }

    @Override
    public String toString() {
        return getOriginalExpr();
    }
}
