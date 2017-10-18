package com.ilscipio.scipio.setup;

import java.io.Serializable;
import java.lang.reflect.Constructor;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.service.LocalDispatcher;

import com.ilscipio.scipio.setup.SetupWorker.StaticSetupWorker.StaticStepState;

/**
 * SCIPIO: Setup worker.
 * NOT thread-safe.
 * WARN: due to caching, DB and other exceptions can come from basically any method.
 */
@SuppressWarnings("serial")
public abstract class SetupWorker implements Serializable {

    public static final String module = SetupWorker.class.getName();
    
    /* 
     * *******************************************
     * Static fields
     * *******************************************
     */
    
    // NOTE: all request attributes should be read/set through SetupWorker instance methods
    // TODO: REVIEW: some of the request/param attributes were implemented before the StaticSetupWorker
    // caching which is superior; maybe some could be eliminated.
    
    /**
     * "Next" setup step that can be requested via req params or attributes (is verified for valid).
     */
    protected static final String REQ_STEP_ATTR = "scpSetupStep";
    
    /**
     * The step we are submitting form for ("last" step).
     */
    protected static final String SUBMIT_STEP_ATTR = "scpSubmitSetupStep";
    
    /**
     * Effective setup step, cached in request after event or first access.
     */
    protected static final String EFF_STEP_ATTR = "scpEffSetupStep";
    
    protected static final String EFF_STEPSTATES_ATTR = "scpEffSetupStepStates";

    // REMOVED: managed to avoid session marks for skipping for now - keeping simple
    //protected static final String SKIPPEDSTEPS_ATTR = "scpSkippedSetupStates";

    /**
     * Name of req attribute that holds a StaticSetupWorker for caching.
     * <p>
     * WARN: This must not be stored in the session; the RequestSetupWorker now tries to prevent RequestHandler
     * from doing this. See {@link RequestSetupWorker#getEnsureStaticWorker}.
     */
    protected static final String STATIC_WORKER_ATTR = "scpStaticSetupWorker";
    
    /**
     * Full list of visible steps and their order.
     * NOTE: see Step States further below for specific implementations.
     */
    private static final List<String> stepsList = UtilMisc.unmodifiableArrayList(
            "organization", "accounting", "facility", "store", "catalog", "user"
            // "website" // merged into store step
    );
    /**
     * Unordered set of the visible steps.
     */
    private static final Set<String> stepsSet = Collections.unmodifiableSet(new HashSet<String>(stepsList));
    
    public static final String FINISHED_STEP = "finished";
    public static final String ERROR_STEP = "error";
    
    /**
     * If orgPartyId set, it uses that as "orgPartyId". Otherwise falls back on the next.
     * NOTE: in the internal param map, 
     */
    private static final List<String> ORGPARTYID_ATTRLIST = UtilMisc.unmodifiableArrayList(
            "orgPartyId", "partyId"
    );
    protected static final String ORGPARTYID_ATTR = "orgPartyId";
    
    /**
     * If orgProductStoreId set, it uses that as "productStoreId". Otherwise falls back on the next.
     */
    private static final List<String> PRODUCTSTOREID_ATTRLIST = UtilMisc.unmodifiableArrayList(
            "orgProductStoreId", "productStoreId"
    );
    protected static final String PRODUCTSTOREID_ATTR = "productStoreId";
    
    protected static final List<String> ALL_PARAM_ATTR;
    static {
        Set<String> all = new LinkedHashSet<>();
        all.addAll(ORGPARTYID_ATTRLIST);
        all.addAll(PRODUCTSTOREID_ATTRLIST);
        ALL_PARAM_ATTR = Collections.unmodifiableList(new ArrayList<>(all));
    }
    
    private static final Set<String> specialStepValues = UtilMisc.unmodifiableHashSet(
            FINISHED_STEP, ERROR_STEP
    );
    
    private static final Set<String> allStepValues;
    static {
        Set<String> s = new HashSet<>(stepsSet);
        s.addAll(specialStepValues);
        allStepValues = Collections.unmodifiableSet(s);
    }
    
    private static final List<String> stepsAndFinList;
    static {
        List<String> list = new ArrayList<>(stepsList.size() + 1);
        list.addAll(stepsList);
        list.add(FINISHED_STEP);
        stepsAndFinList = Collections.unmodifiableList(list);
    }
    private static final Set<String> stepsAndFinSet = Collections.unmodifiableSet(new HashSet<String>(stepsAndFinList));
    
    private static final String initialStep = stepsList.get(0);

    private static final Set<String> reqAttrNamesToSkip = UtilMisc.unmodifiableHashSet(
            STATIC_WORKER_ATTR, "servletContext");
    
    /* 
     * *******************************************
     * Constructors
     * *******************************************
     */
    
    public static SetupWorker getWorker(HttpServletRequest request, boolean useReqCache) {
        return RequestSetupWorker.getWorker(request, useReqCache);
    }
    
    public static SetupWorker getWorker(HttpServletRequest request) {
        return RequestSetupWorker.getWorker(request, true);
    }
    
    public static void clearCached(HttpServletRequest request) {
        request.removeAttribute(STATIC_WORKER_ATTR);
    }
    
    /* 
     * *******************************************
     * Stateless info
     * *******************************************
     */
    
    public List<String> getSteps() { return stepsList; }
    public Set<String> getStepsSet() { return stepsSet; }
    public int getStepsCount() { return stepsList.size(); }
    public Set<String> getAllStepValues() { return allStepValues; }
    public List<String> getStepsAndFinished() { return stepsAndFinList; }
    public Set<String> getStepsAndFinishedSet() { return stepsAndFinSet; }
    public int getStepsAndFinishedCount() { return stepsAndFinList.size(); }
    public String getInitialStep() { return initialStep; }
    
    public int getStepIndex(String step) { return stepsList.indexOf(step); }
    public int getStepIndexValid(String step) throws IllegalArgumentException { 
        int stepIndex = getStepIndex(step);
        if (stepIndex < 0) throw new IllegalArgumentException("invalid setup step: " + step);
        return stepIndex;
    }
    public int getStepIndexWithFinished(String step) { return FINISHED_STEP.equals(step) ? stepsList.size() : stepsList.indexOf(step); }
    public int getStepIndexWithFinishedValid(String step) throws IllegalArgumentException { 
        int stepIndex = getStepIndexWithFinished(step);
        if (stepIndex < 0) throw new IllegalArgumentException("invalid setup step: " + step);
        return stepIndex;
    }
    
    public List<String> getStepsBefore(String step) throws IllegalArgumentException {
        if (FINISHED_STEP.equals(step)) return getSteps();
        return getSteps().subList(0, getStepIndexValid(step));
    }
    public List<String> getStepsBeforeIncluding(String step) throws IllegalArgumentException {
        if (FINISHED_STEP.equals(step)) return getSteps();
        return getSteps().subList(0, getStepIndexValid(step) + 1);
    }
    
    public String getStepBefore(String step) throws IllegalArgumentException {
        int index = getStepIndexValid(step) - 1;
        return (index >= 0) ? getSteps().get(index) : null;
    }
    
    /**
     * Returns step after OR "finished".
     */
    public String getStepAfter(String step) throws IllegalArgumentException {
        if (FINISHED_STEP.equals(step)) return null;
        int index = getStepIndexValid(step) + 1;
        List<String> steps = getSteps();
        return (index < steps.size()) ? steps.get(index) : FINISHED_STEP;
    }
    
    public boolean isValidStep(String step) { return getStepsSet().contains(step); }
    public boolean isValidStepOrFinished(String step) { return getStepsSet().contains(step) || FINISHED_STEP.equals(step); }
    
    public void checkValidStep(String step) throws IllegalArgumentException {
        if (!isValidStep(step)) throw new IllegalArgumentException("invalid setup step: " + step);
    }
    public void checkValidStepOrFinished(String step) throws IllegalArgumentException {
        if (!isValidStepOrFinished(step)) throw new IllegalArgumentException("invalid setup step: " + step);
    }
    
    public boolean isStepEqualOrComesAfter(String beforeStep, String step) {
        return getStepIndexWithFinished(step) >= getStepIndexWithFinished(beforeStep);
    }
    
    public static List<String> getStepsStatic() {
        return stepsList;
    }
    
    /* 
     * *******************************************
     * Step state base interfaces
     * *******************************************
     */

    /**
     * Step state info.
     * NOT thread-safe.
     */
    public static abstract class StepState implements Serializable {
        
        /*
         * Standard fields (part of toMap)
         */
        
        public abstract String getName();
        public abstract boolean isSkippable();
        public abstract boolean isCompleted();
        public abstract boolean isCoreCompleted();
        public abstract boolean isAccessible();
        // REMOVED: managed to avoid session marks for skipping for now - keeping simple
        //public abstract boolean isSkipped();
        public abstract Map<String, Object> getStepData();
        /**
         * The params that should be passed to this step to access it.
         */
        public abstract Map<String, Object> getStepParams();
        
        /*
         * Extra fields and helpers (not part of toMap())
         */
        
        public abstract StepParamInfo getStepParamInfo();
        /**
         * Returns true if the step is effectively skippable in context,
         * even if {@link #isSkippable} returns false.
         */
        public abstract boolean isSkippableEffective();
        
        public abstract String getNextAccessibleStep();
        
        // does not work out due to lack of encoding
//        /**
//         * Makes a RAW unescaped query string.
//         */
//        public String getStepQueryString() {
//            StringBuilder sb = new StringBuilder(); // FIXME: there's an API somewhere...
//            Map<String, Object> params = getStepParams();
//            if (params != null) {
//                for(Map.Entry<String, Object> entry : params.entrySet()) {
//                    sb.append("&");
//                    sb.append(entry.getKey());
//                    sb.append("=");
//                    Object value = entry.getValue();
//                    if (value != null) {
//                        sb.append(value.toString());
//                    }
//                }
//            }
//            return (sb.length() > 0) ? sb.substring(1) : "";
//        }
        
        public void toMap(Map<String, Object> map) {
            map.put("name", getName());
            map.put("stepData", getStepData());
            map.put("skippable", isSkippable());
            map.put("completed", isCompleted());
            // REMOVED: managed to avoid session marks for skipping for now - keeping simple
            //map.put("skipped", isSkipped());
            map.put("accessible", isAccessible());
            map.put("disabled", !isAccessible());
            map.put("stepParams", getStepParams());
            // can't do this
            //map.put("stepQueryString", getStepQueryString());
            // TODO?: not needed yet...
            //map.put("stepParamInfo", getStepParamInfo().toMap());
        }
        public Map<String, Object> toMap() {
            Map<String, Object> map = new HashMap<>();
            toMap(map);
            return map;
        }
    }
    
    public static class StepParamInfo implements Serializable {
        private final Set<String> required;
        private final Set<String> optional;
        private final Set<String> supported;
        private StepParamInfo(Set<String> required, Set<String> optional, Set<String> supported) {
            this.required = required;
            this.optional = optional;
            this.supported = supported;
        }
        
        public static StepParamInfo fromRequiredAndOptional(Set<String> required, Set<String> optional) {
            return new StepParamInfo(Collections.unmodifiableSet(required), 
                    Collections.unmodifiableSet(optional),
                    Collections.unmodifiableSet(combineSets(required, optional)));
        }
        
        public Set<String> getRequired() { return required; }
        public Set<String> getOptional() { return optional; }
        public Set<String> getSupported() { return supported; }
        
        @SafeVarargs
        public static Set<String> combineSets(Set<String>... sets) {
            Set<String> res = new HashSet<>();
            for(Set<String> set : sets) {
                res.addAll(set);
            }
            return res;
        }
        
        public static Set<String> nameSet(String... names) {
            return UtilMisc.toHashSet(names);
        }
        
        public void toMap(Map<String, Object> map) {
            map.put("required", getRequired());
            map.put("optional", getOptional());
            map.put("supported", getSupported());
        }
        public Map<String, Object> toMap() {
            Map<String, Object> map = new HashMap<>();
            toMap(map);
            return map;
        }
    }
    
    /* 
     * *******************************************
     * Stateful queries
     * *******************************************
     */
    
    public abstract void clearCached();
    
    /**
     * Returns a validated and normalized parameter from request attributes or parameters.
     * Name can be: orgPartyId, productStoreId, ...
     */
    public abstract Object getValidParam(String name);
    
    /**
     * Returns a NON-validated but preprocessed parameter from request attributes or parameters.
     * Name can be: orgPartyId, productStoreId, ...
     */
    public abstract Object getParam(String name);
    
    /**
     * Returns validated orgPartyId, or null if invalid.
     * NOTE: may be empty at first step
     */
    public String getOrgPartyId() {
        return (String) getValidParam("orgPartyId");
    }
    
    /**
     * Returns validated productStoreId, or null if invalid.
     * NOTE: may be empty before first step
     */
    public String getProductStoreId() {
        return (String) getValidParam("productStoreId");
    }
    
    /**
     * Helper to read current org party; only non-null if valid.
     */
    public abstract GenericValue getOrgParty();
    
    /**
     * Helper to read current org party; only non-null if valid.
     */
    public abstract GenericValue getOrgPartyGroup();
    
    /**
     * Helper to read current product store; only non-null if valid.
     */
    public abstract GenericValue getProductStore();
    
    /**
     * Returns last submitted setup step (rather than "current"/"next").
     */
    public abstract String getSubmittedStep();
    
    /**
     * Returns the requested step attrib/param only.
     * WARN: must be verified.
     */
    public abstract String getRequestedStep();
    
    /**
     * Returns the effective step attrib/param only.
     * WARN: must be verified.
     */
    public abstract String getEffectiveStep();
    
    /**
     * WARN: Call before everything else.
     * May trigger cache clear (subject to change - see implementation).
     */
    public abstract void setEffectiveStep(String effectiveStep);
    
    /**
     * Returns the "current"/"next" step.
     * In all cases, if the EFF_SETUP_STEP_ATTR is set, everything else is ignored.
     * <p>
     * NOTE: Upon review, screens don't need to call this (only events); they set a context var and just
     * need to validate it with {@link #isStepEffectiveSafe}.
     * <p>
     * If submitted step is used, whether to stay or go to next step is controlled by the
     * <code>setupContinue=Y/N</code> request parameter (default: Y).
     * <p>
     * @param useRequested if TRUE check requested and error if invalid; if FALSE ignore requested; 
     *                     if null check requested but fallback if invalid
     * @param useSubmitted if TRUE or null check submitted step and return the next step (depending on setupContinue Y/N parameter; 
     *                     if FALSE ignore submitted;
     *                     if the next step is not accessible, returns the next auto-determined step that must be filled
     * @param updateEffective if TRUE always update effective even if exception (sets "error"); if FALSE never update;
     *                        if null only update if non-exception
     * @see #getSubmittedStep()
     */
    public abstract String determineStepAndUpdate(Boolean useRequested, Boolean useSubmitted, Boolean updateEffective);
    
    /**
     * Full-featured determineStepAndUpdate call with write-back to effective, 
     * same as <code>determineStep(Boolean.TRUE, Boolean.TRUE, Boolean.TRUE)</code>.
     * <p>
     * NOTE: Upon review, screens don't need to call this (only events); they set a context var and just
     * need to validate it with {@link #isSetupStepEffectiveSafe}.
     */
    public abstract String determineStepAndUpdate();
    
    /**
     * Read-only
     */
    public abstract String determineStep(Boolean useRequested, Boolean useSubmitted);
    
    public abstract String determineStepAuto(boolean useCache);
    
    public String determineStepAuto() {
        return determineStepAuto(true);
    }
    
    public String getNextAccessibleStep(String step) {
        return getStepState(step).getNextAccessibleStep();
    }
    
    // REMOVED: managed to avoid session marks for skipping for now - keeping simple
//    /**
//     * WARN: Call before everything else.
//     * May trigger cache clear (subject to change - see implementation).
//     */
//    public abstract void markStepSkipped(String step) throws IllegalArgumentException, IllegalStateException;

    public abstract boolean isStepAllowed(String requestedStep) throws IllegalArgumentException;
    
    /**
     * If the effective is set, checks if the step matches it; if effective is not set, checks if the step is allowed.
     * FOR USE IN SCREENS to prevent bypassing the wizard.
     */
    public abstract boolean isStepEffectiveAllowedSafe(String step);
    
    public abstract StepState getStepState(String step) throws IllegalArgumentException;

    /**
     * Returns all step states by step name.
     * NOTE: Individual StepStates may lazily resolve values. 
     */
    public abstract Map<String, StepState> getStepStateMap() throws IllegalArgumentException;
    
    /**
     * Returns a map of step names to submaps with keys (all pre-resolved values) and caches in request:
     * accessible (Boolean), skippable (Boolean), skipped (Boolean), completed (Boolean)
     * NOTE: Forces all values to resolve. 
     * Only for use in screens after all events processed.
     */
    public abstract Map<String, Map<String, Object>> getStepStatePrimitiveMap() throws IllegalArgumentException;

    public abstract boolean isAllStepsCompleted();
    
    public abstract List<String> getIncompleteSteps();
    
    // Exact request states 
    
    /**
     * Returns true if new record form requested - this controls if form should show create or update.
     */
    public abstract boolean isNewRecordRequest(String step);
    /**
     * Returns true if a create form was submitted.
     * NOTE: when this is true, isNewRecordRequest may return false because only one of these gets
     * passed. You may need check both.
     */
    public abstract boolean isCreateRecordRequest(String step);
    public abstract boolean isFailedCreateRecordRequest(String step);
    public abstract boolean isDeleteRecordRequest(String step);
    public abstract boolean isSuccessDeleteRecordRequest(String step);
    
    // Aggregate/high-level states
    
    public abstract boolean isUnspecificRecordRequest(String step);
    
    /**
     * This is basically isNewRecordRequest+isFailedCreateRecordRequest for now.
     * Couldn't find a better name.
     */
    public abstract boolean isEffectiveNewRecordRequest(String step);
    
    
    /* 
     * *******************************************
     * Static instance worker
     * *******************************************
     */
    
    /**
     * Holds the actual resolved values but NOT HttpServletRequest so that
     * it can be stored and cached inside HttpServletRequest attributes.
     * <p>
     * The {@link RequestSetupWorker} keeps an instance same as one in request attribs 
     * and modifies it directly so the one stored in HttpServletRequest is always up to date.
     * TODO?: the UnsupportedOperationException below are because this is not exposed
     * to client code at the moment - implement as-needed.
     */
    protected static class StaticSetupWorker extends SetupWorker {
        private Map<String, StepState> stepStateMap = new HashMap<>();
        private String autoDetStep = null;
        /**
         * NON-validated parameters.
         */
        private Map<String, Object> params = null; 
        /**
         * Validated parameters.
         */
        private Map<String, Object> validParams = null;
        
        @Override
        public void clearCached() {
        }
        @Override
        public Object getValidParam(String name) {
            return validParams.get(name);
        }
        @Override
        public Object getParam(String name) {
            return params.get(name);
        }
        @Override
        public GenericValue getOrgParty() {
            throw new UnsupportedOperationException(); // TODO?
        }
        @Override
        public GenericValue getOrgPartyGroup() {
            throw new UnsupportedOperationException(); // TODO?
        }
        @Override
        public GenericValue getProductStore() {
            throw new UnsupportedOperationException(); // TODO?
        }
        @Override
        public String getSubmittedStep() {
            throw new UnsupportedOperationException(); // TODO?
        }
        @Override
        public String getRequestedStep() {
            throw new UnsupportedOperationException(); // TODO?
        }
        @Override
        public String getEffectiveStep() {
            throw new UnsupportedOperationException(); // TODO?
        }
        @Override
        public void setEffectiveStep(String effectiveStep) {
            throw new UnsupportedOperationException(); // TODO?
        }
        @Override
        public String determineStepAndUpdate(Boolean useRequested, Boolean useSubmitted, Boolean updateEffective) {
            throw new UnsupportedOperationException(); // TODO?
        }
        @Override
        public String determineStepAndUpdate() {
            throw new UnsupportedOperationException(); // TODO?
        }
        @Override
        public String determineStep(Boolean useRequested, Boolean useSubmitted) {
            throw new UnsupportedOperationException(); // TODO?
        }
        @Override
        public String determineStepAuto(boolean useCache) {
            throw new UnsupportedOperationException(); // TODO?
        }
//        @Override
//        public void markStepSkipped(String step) throws IllegalArgumentException {
//            throw new UnsupportedOperationException(); // TODO?
//        }
        @Override
        public boolean isStepAllowed(String requestedStep) throws IllegalArgumentException {
            throw new UnsupportedOperationException(); // TODO?
        }
        @Override
        public boolean isStepEffectiveAllowedSafe(String step) {
            throw new UnsupportedOperationException(); // TODO?
        }
        @Override
        public StepState getStepState(String step) throws IllegalArgumentException {
            return stepStateMap.get(step);
        }
        @Override
        public Map<String, StepState> getStepStateMap() throws IllegalArgumentException {
            return Collections.unmodifiableMap(stepStateMap);
        }
        @Override
        public Map<String, Map<String, Object>> getStepStatePrimitiveMap() throws IllegalArgumentException {
            Map<String, Map<String, Object>> stateMap = new HashMap<>();
            for(String step : getStepsAndFinished()) {
                stateMap.put(step, Collections.unmodifiableMap(getStepState(step).toMap()));
            }
            return Collections.unmodifiableMap(stateMap);
        }
        @Override
        public boolean isAllStepsCompleted() {
            throw new UnsupportedOperationException(); // TODO?
        }
        @Override
        public List<String> getIncompleteSteps() {
            throw new UnsupportedOperationException(); // TODO?
        }
        @Override
        public boolean isNewRecordRequest(String step) {
            throw new UnsupportedOperationException(); // TODO?
        }
        @Override
        public boolean isCreateRecordRequest(String step) {
            throw new UnsupportedOperationException(); // TODO?
        }
        @Override
        public boolean isFailedCreateRecordRequest(String step) {
            throw new UnsupportedOperationException(); // TODO?
        }
        @Override
        public boolean isDeleteRecordRequest(String step) {
            throw new UnsupportedOperationException(); // TODO?
        }
        @Override
        public boolean isSuccessDeleteRecordRequest(String step) {
            throw new UnsupportedOperationException(); // TODO?
        }
        @Override
        public boolean isUnspecificRecordRequest(String step) {
            throw new UnsupportedOperationException(); // TODO?
        }
        @Override
        public boolean isEffectiveNewRecordRequest(String step) {
            throw new UnsupportedOperationException(); // TODO?
        }
        protected static class StaticStepState extends StepState { // WARN: this one MUST be static
            protected String name;
            // NOTE: are boxed types instead of primitives due to reuse for caching
            protected Boolean skippable;
            protected Boolean completed;
            protected Boolean coreCompleted;
            //protected Boolean skipped;
            protected Boolean accessible;
            protected Map<String, Object> stepData;
            protected Map<String, Object> stepParams;
            protected boolean resolved; // FIXME: handle this flag better - it's only set in subset of possible cases, for opt
            
            protected StaticStepState() { ; }
            
            public StaticStepState(StepState other) {
                this.name = other.getName();
                this.skippable = other.isSkippable();
                this.completed = other.isCompleted();
                this.coreCompleted = other.isCoreCompleted();
                //this.skipped = other.isSkipped();
                this.accessible = other.isAccessible();
                this.stepData = other.getStepData();
                this.stepParams = other.getStepParams();
                this.resolved = true; // the calls above enforce this
            }
            
            public StaticStepState(StaticStepState partial, StepState other) {
                this.name = partial.name != null ? partial.name : other.getName();
                this.skippable = partial.skippable != null ? partial.skippable : other.isSkippable();
                this.completed = partial.completed != null ? partial.completed : other.isCompleted();
                this.coreCompleted = partial.coreCompleted != null ? partial.coreCompleted : other.isCoreCompleted();
                //this.skipped = partial.skipped != null ? partial.skipped : other.isSkipped();
                this.accessible = partial.accessible != null ? partial.accessible : other.isAccessible();
                this.stepData = partial.stepData != null ? partial.stepData : other.getStepData();
                this.stepParams = partial.stepParams != null ? partial.stepParams : other.getStepParams();
                this.resolved = true; // the calls above enforce this
            }
            
            protected StaticStepState(StaticStepState partial, boolean partialIsComplete) {
                this.name = partial.name;
                this.skippable = partial.skippable;
                this.completed = partial.completed;
                this.coreCompleted = partial.coreCompleted;
                //this.skipped = partial.skipped;
                this.accessible = partial.accessible;
                this.stepData = partial.stepData;
                this.stepParams = partial.stepParams;
                this.resolved = partial.resolved;
            }

            @Override public String getName() { return name; }
            @Override public boolean isSkippable() { return skippable; }
            @Override public boolean isCompleted() { return completed; }
            @Override public boolean isCoreCompleted() { return coreCompleted; }
            //@Override public boolean isSkipped() { return skipped; }
            @Override public boolean isAccessible() { return accessible; }
            @Override public Map<String, Object> getStepData() { return stepData; }
            @Override public Map<String, Object> getStepParams() { return stepParams; }
            
            protected boolean isResolved() { return resolved; }
            protected void markResolved() { resolved = true; }

            @Override
            public StepParamInfo getStepParamInfo() {
                throw new UnsupportedOperationException(); // TODO? if needed only
            }

            @Override
            public boolean isSkippableEffective() {
                throw new UnsupportedOperationException(); // impossible
            }

            @Override
            public String getNextAccessibleStep() {
                throw new UnsupportedOperationException(); // impossible
            }
        }
    }
    
    /* 
     * *******************************************
     * Core Worker implementation
     * *******************************************
     */
    
    /**
     * Setup worker that wraps a HttpServletRequest and implements the core logic.
     * It caches some info in a {@link StaticSetupWorker} instance that is also
     * set in request so it transfers from events to screens.
     */
    protected static class RequestSetupWorker extends SetupWorker {
  
        private StaticSetupWorker staticWorker;
        private HttpServletRequest request;
        private Delegator delegator = null;
        private LocalDispatcher dispatcher = null;
        /**
         * NOTE: each entry in this map references an entry in staticWorker.stepStateMap,
         * so it auto-populates the cache.
         */
        private Map<String, StepState> stepStateMap = new HashMap<>();
        private List<StepState> stepStateList = null;
        private boolean useReqCache;
        
        protected RequestSetupWorker(HttpServletRequest request, StaticSetupWorker staticWorker, boolean useReqCache) {
            this.request = request;
            this.staticWorker = staticWorker;
            this.useReqCache = useReqCache;
        }

        public static RequestSetupWorker getWorker(HttpServletRequest request, boolean useReqCache) {
            return new RequestSetupWorker(request, getEnsureStaticWorker(request, useReqCache), useReqCache);
        }
        
        protected static StaticSetupWorker getEnsureStaticWorker(HttpServletRequest request, boolean useReqCache) {
            if (useReqCache) {
                StaticSetupWorker staticWorker = (StaticSetupWorker) request.getAttribute(STATIC_WORKER_ATTR);
                if (staticWorker == null) {
                    staticWorker = new StaticSetupWorker();
                    request.setAttribute(STATIC_WORKER_ATTR, staticWorker);
                    
                    // SPECIAL: this prevents the cached data from getting saved into the session
                    Set<String> saveViewAttrExcl = UtilGenerics.checkSet(request.getAttribute("_SCP_VIEW_SAVE_ATTR_EXCL_"));
                    if (saveViewAttrExcl == null) {
                        saveViewAttrExcl = new HashSet<>();
                    }
                    saveViewAttrExcl.add(STATIC_WORKER_ATTR);
                    request.setAttribute("_SCP_VIEW_SAVE_ATTR_EXCL_", saveViewAttrExcl);
                }
                return staticWorker;
            } else {
                return new StaticSetupWorker();
            }
        }
        
        @Override
        public void clearCached() {
            if (useReqCache) SetupWorker.clearCached(request);
            this.staticWorker = getEnsureStaticWorker(request, useReqCache);
        }
        
        protected Map<String, StepState> getStepStateMapPriv() {
            return stepStateMap;
        }
        
        @Override
        public String getSubmittedStep() {
            String lastStep = (String) request.getAttribute(SUBMIT_STEP_ATTR);
            if (lastStep == null) {
                lastStep = request.getParameter(SUBMIT_STEP_ATTR);
            }
            return lastStep;
        }

        @Override
        public String getRequestedStep() {
            String step = (String) request.getAttribute(REQ_STEP_ATTR);
            if (step == null) {
                step = request.getParameter(REQ_STEP_ATTR);
            }
            return step;
        }
        
        @Override
        public String getEffectiveStep() {
            return (String) request.getAttribute(EFF_STEP_ATTR);
        }
        
        @Override
        public void setEffectiveStep(String effectiveStep) {
            try {
                request.setAttribute(EFF_STEP_ATTR, effectiveStep);
            } finally {
                // TODO: REVIEW: for now there's no code that appears to require a cache clear.
                // this could change at any time...
                //if (!strEquals(effectiveStep, getEffectiveStep())) {
                //    this.clearCached();
                //}
            }
        }
        
        @Override
        public String determineStepAndUpdate(Boolean useRequested, Boolean useSubmitted, Boolean updateEffective) {
            String step = getEffectiveStep();
            if (step == null) {
                try {
                    step = determineStep(useRequested, useSubmitted);
                    if (!Boolean.FALSE.equals(updateEffective)) setEffectiveStep(step);
                } catch(Exception e) {
                    if (Boolean.TRUE.equals(updateEffective)) setEffectiveStep(ERROR_STEP);
                    throw e;
                }
            }
            return step;
        }
        
        @Override
        public String determineStepAndUpdate() {
            return determineStepAndUpdate(Boolean.TRUE, Boolean.TRUE, Boolean.TRUE);
        }
        
        @Override
        public String determineStep(Boolean useRequested, Boolean useSubmitted) {
            String requestedStep = !Boolean.FALSE.equals(useRequested) ? getRequestedStep() : null;
            if (UtilValidate.isNotEmpty(requestedStep)) {
                if (Boolean.TRUE.equals(useRequested)) { 
                    if (isStepAllowed(requestedStep)) return requestedStep;
                    else throw new IllegalArgumentException("setup step requested cannot be fulfilled (prior steps required): " + requestedStep);
                } else {
                    try {
                        if (isStepAllowed(requestedStep)) return requestedStep;
                    } catch(Exception e) {
                        ;
                    }
                }
            } 
            
            if (!Boolean.FALSE.equals(useSubmitted)) {
                // SUBMIT HANDLING: if a step is submitted, you usually expect to go to the next one,
                // even if prior steps were skipped. so we always move to the next step 
                // AS LONG AS it's accessible from here. if not, don't do error like the requestedStep
                // because this is part of the automatic process
                String submittedStep = getSubmittedStep();
                if (UtilValidate.isNotEmpty(submittedStep)) {
                    if (Boolean.FALSE.equals(UtilMisc.booleanValueVersatile(getParams().get("setupContinue")))) {
                        if (isStepAllowed(submittedStep)) return submittedStep;
                        else {
                            Debug.logWarning("Setup: The submitted setup step (" + submittedStep
                                    + ") is not re-accessible for some reason"
                                    + "; returning auto-determined next step instead", module);
                        }
                    } else {
                        String nextStep = getNextAccessibleStep(submittedStep);
                        if (nextStep != null) return nextStep;
                        else {
                            Debug.logInfo("Setup: There is no accessible setup step after the submitted step (" 
                                    + submittedStep + "); returning auto-determined next step instead", module);
                        }
                    }
                }
            }

            return determineStepAuto(true);
        }
        
        @Override
        public String determineStepAuto(boolean useCache) {
            String step = staticWorker.autoDetStep;
            if (step == null || !useCache) {
                for(StepState state : getStepStatesInternal()) {
                    // REMOVED: managed to avoid session marks for skipping for now - keeping simple
                    //if (!state.isCompleted() && !state.isSkipped()) {
                    if (!state.isCompleted()) {
                        step = state.getName();
                        break;
                    }
                }
                if (step == null) step = FINISHED_STEP;
                if (useCache) staticWorker.autoDetStep = step;
            }
            return step;
        }
        
        // REMOVED: managed to avoid session marks for skipping for now - keeping simple
//        private Map<String, Boolean> getSkippedSteps() {
//            Map<String, Boolean> skippedSteps = UtilGenerics.checkMap(request.getAttribute(SKIPPEDSTEPS_ATTR));
//            if (skippedSteps == null) {
//                // NOTE: we only ever read once from session then use request thereafter
//                skippedSteps = UtilGenerics.checkMap(getSession().getAttribute(SKIPPEDSTEPS_ATTR));
//                if (skippedSteps == null) skippedSteps = Collections.emptyMap();
//                // set in request to avoid sync issues
//                request.setAttribute(SKIPPEDSTEPS_ATTR, skippedSteps);
//            }
//            return skippedSteps;
//        }
        
        // REMOVED: managed to avoid session marks for skipping for now - keeping simple
//        /**
//         * Marks step skipped in session regardless of whether skippable(!).
//         */
//        protected void markStepSkippedForce(String step) throws IllegalArgumentException {     
//            Map<String, Boolean> skippedSteps = getSkippedSteps();
//            if (!Boolean.TRUE.equals(skippedSteps.get(step))) {
//                try {
//                    skippedSteps = new HashMap<>(skippedSteps);
//                    skippedSteps.put(step, Boolean.TRUE);
//                    skippedSteps = Collections.unmodifiableMap(skippedSteps); // immutable/thread-safe
//                    request.setAttribute(SKIPPEDSTEPS_ATTR, skippedSteps);
//                    getSession().setAttribute(SKIPPEDSTEPS_ATTR, skippedSteps);
//                } finally {
//                    // SPECIAL: WE MUST CLEAR ALL CACHE AFTER THIS
//                    this.clearCached();
//                }
//            }
//        }
        
//        @Override
//        public void markStepSkipped(String step) throws IllegalArgumentException, IllegalStateException {
//            // VERIFY
//            if (!getStepState(step).isSkippable()) throw new IllegalStateException("step is not skippable: " + step);
//            if (!isStepAllowed(getStepAfter(step))) throw new IllegalStateException("step cannot be skipped (at this point): " + step);
//
//            markStepSkippedForce(step);
//        }
    
        @Override
        public boolean isStepAllowed(String requestedStep) throws IllegalArgumentException {
            if (ERROR_STEP.equals(requestedStep)) return true;
            if (!getAllStepValues().contains(requestedStep)) {
                throw new IllegalArgumentException("invalid setup step requested: " + requestedStep);
            }
            // SIMPLIFIED: use isAccessible instead
//            for(StepState state : getStepStatesInternal()) {
//                if (state.getName().equals(requestedStep)) {
//                    return true;
//                }
//                if (!state.isSkippable() && !state.isCompleted()) {
//                    return false;
//                }
//            }
//            return FINISHED_STEP.equals(requestedStep);
            return getStepState(requestedStep).isAccessible();
        }
        
        @Override
        public boolean isStepEffectiveAllowedSafe(String step) {
            if (UtilValidate.isEmpty(step)) return true;
            try {
                String effStep = getEffectiveStep();
                // if an eff step was set, we have to match it exactly
                if (effStep != null && !effStep.equals(step)) return false;
                return isStepAllowed(step);
            } catch (Exception e) {
                Debug.logError(e, "Setup: Error determining setup step: " + e.getMessage(), module);
                return false;
            }
        }

        private StepState getNewStepState(String step) {
            Class<?> cls = stepStateClsMap.get(step);
            if (cls == null) throw new IllegalArgumentException("invalid setup step: " + step);
            Constructor<?> ctor;
            try {
                StaticStepState partialState = (StaticStepState) staticWorker.stepStateMap.get(step);
                if (partialState == null) {
                    partialState = new StaticStepState();
                    staticWorker.stepStateMap.put(step, partialState);
                }
                
                ctor = cls.getDeclaredConstructor(RequestSetupWorker.class, StaticStepState.class);
                return (StepState) ctor.newInstance(this, partialState);
            } catch (Exception e) {
                Debug.logError(e, module);
                throw new IllegalArgumentException("invalid setup step: " + step);
            }
        }
        
        @Override
        public StepState getStepState(String step) throws IllegalArgumentException {
            StepState stepState = getStepStateMapPriv().get(step);
            if (stepState == null) {
                stepState = getNewStepState(step);
                getStepStateMapPriv().put(step, stepState);
            }
            return stepState;
        }
        
        private Map<String, StepState> getStepStateMapLoaded() {
            if (getStepStateMapPriv().size() < getStepsAndFinished().size()) {
                for(String step : getStepsAndFinished()) {
                    getStepState(step);
                }
            }
            return getStepStateMapPriv();
        }
        
        private List<StepState> getStepStatesInternal() {
            if (stepStateList == null) {
                // WARN: DOES NOT include finished
                List<StepState> stepStateList = new ArrayList<>(getSteps().size());
                for(String step : getSteps()) {
                    StepState stepState = getStepState(step);
                    stepStateList.add(stepState);
                }
                this.stepStateList = Collections.unmodifiableList(stepStateList);
            }
            return stepStateList;
        }
        
        @Override
        public Map<String, StepState> getStepStateMap() {
            return Collections.unmodifiableMap(getStepStateMapLoaded());
        }

        @Override
        public Map<String, Map<String, Object>> getStepStatePrimitiveMap() throws IllegalArgumentException {
            Map<String, Map<String, Object>> stateMap = new HashMap<>();
            for(String step : getStepsAndFinished()) {
                stateMap.put(step, Collections.unmodifiableMap(getStepState(step).toMap()));
            }
            return Collections.unmodifiableMap(stateMap);
        }

        @Override
        public boolean isAllStepsCompleted() {
            for(StepState stepState : getStepStatesInternal()) {
                if (!stepState.isCompleted()) return false;
            }
            return true;
        }
        @Override
        public List<String> getIncompleteSteps() {
            List<String> missing = new ArrayList<>();
            for(StepState stepState : getStepStatesInternal()) {
                if (!stepState.isCompleted()) missing.add(stepState.getName());
            }
            return Collections.unmodifiableList(missing);
        }
        
        @Override
        public boolean isNewRecordRequest(String step) {
            return SetupDataUtil.isNewRecordRequest(getParams(), step.substring(0, 1).toUpperCase() + step.substring(1));
        }
        @Override
        public boolean isCreateRecordRequest(String step) {
            return SetupDataUtil.isCreateRecordRequest(getParams(), step.substring(0, 1).toUpperCase() + step.substring(1));
        }
        @Override
        public boolean isFailedCreateRecordRequest(String step) {
            return SetupDataUtil.isFailedCreateRecordRequest(getParams(), step.substring(0, 1).toUpperCase() + step.substring(1));
        }
        @Override
        public boolean isDeleteRecordRequest(String step) {
            return SetupDataUtil.isDeleteRecordRequest(getParams(), step.substring(0, 1).toUpperCase() + step.substring(1));
        }
        @Override
        public boolean isSuccessDeleteRecordRequest(String step) {
            return SetupDataUtil.isSuccessDeleteRecordRequest(getParams(), step.substring(0, 1).toUpperCase() + step.substring(1));
        }
        
        @Override
        public boolean isUnspecificRecordRequest(String step) {
            return SetupDataUtil.isUnspecificRecordRequest(getParams(), step.substring(0, 1).toUpperCase() + step.substring(1));
        }
        @Override
        public boolean isEffectiveNewRecordRequest(String step) {
            return SetupDataUtil.isEffectiveNewRecordRequest(getParams(), step.substring(0, 1).toUpperCase() + step.substring(1));
        }
        
        /* 
         * *******************************************
         * Step-specific implementations
         * *******************************************
         */
    
        private static final Map<String, Class<?>> stepStateClsMap;
        static {
            Map<String, Class<?>> m = new HashMap<>();
            
            m.put("organization", OrganizationStepState.class);
            m.put("user", UserStepState.class);
            m.put("accounting", AccountingStepState.class);
            m.put("facility", FacilityStepState.class);
            m.put("catalog", CatalogStepState.class);
            m.put("store", StoreStepState.class);
            //m.put("website", WebsiteStepState.class); // merged into store step
            
            // pseudo-steps
            m.put(FINISHED_STEP, FinishedStepState.class);
            
            stepStateClsMap = Collections.unmodifiableMap(m);
        }

        /**
         * Implements step state logic and populates a StaticStepState instance in parallel.
         */
        protected abstract class CommonStepState extends StepState {
            
            protected final StaticStepState staticState;
            
            public CommonStepState(StaticStepState staticState) { this.staticState = staticState; }
    
            public StaticStepState getPartialStaticState() {
                return staticState;
            }
            
            public StaticStepState getResolvedStaticState() {
                if (staticState.isResolved()) return staticState;
                else {
                    resolve();
                    return staticState;
                }
            }
            
            public void resolve() {
                getStepData();
                getStepParams();
                isSkippable();
                isCompleted();
                isCoreCompleted();
                // REMOVED: managed to avoid session marks for skipping for now - keeping simple
                //isSkipped();
                isAccessible();
                staticState.markResolved();
            }
            
            // delegating implementations
            
            @Override 
            public boolean isSkippable() { 
                if (staticState.skippable == null) {
                    try {
                        staticState.skippable = isSkippableImpl();
                    } catch (GeneralException e) {
                        handleEx(e);
                        staticState.skippable = false; // prevents multi-failures
                    }
                } 
                return staticState.skippable;
            }
            @Override 
            public boolean isCompleted() { 
                if (staticState.completed == null) {
                    try {
                        staticState.completed = isCompletedImpl();
                    } catch (GeneralException e) {
                        handleEx(e);
                        staticState.completed = false; // prevents multi-failures
                    }
                } 
                return staticState.completed;
            }
            @Override 
            public boolean isCoreCompleted() { 
                if (staticState.coreCompleted == null) {
                    try {
                        staticState.coreCompleted = isCoreCompletedImpl();
                    } catch (GeneralException e) {
                        handleEx(e);
                        staticState.coreCompleted = false; // prevents multi-failures
                    }
                } 
                return staticState.coreCompleted;
            }
//            @Override 
//            public boolean isSkipped() { 
//                if (staticState.skipped == null) {
//                    try {
//                        staticState.skipped = isSkippedImpl();
//                    } catch (GeneralException e) {
//                        handleEx(e);
//                        staticState.skipped = false; // prevents multi-failures
//                    }
//                } 
//                return staticState.skipped;
//            }
            @Override 
            public boolean isAccessible() { 
                if (staticState.accessible == null) {
                    try {
                        staticState.accessible = isAccessibleImpl();
                    } catch (GeneralException e) {
                        handleEx(e);
                        staticState.accessible = false; // prevents multi-failures
                    }
                } 
                return staticState.accessible; 
            }
            @Override 
            public Map<String, Object> getStepData() { 
                if (staticState.stepData == null) { 
                    try {
                        staticState.stepData = getStepDataImpl();
                    } catch (GeneralException e) {
                        handleEx(e);
                        staticState.stepData = new HashMap<>(); // prevents multi-failures
                    }
                }
                return staticState.stepData;
            }
            @Override
            public Map<String, Object> getStepParams() { // NOTE: some subclasses may need to override this
                if (staticState.stepParams == null) {
                    try {
                        Map<String, Object> params = new HashMap<>();
                        for(String name : getStepParamInfo().getRequired()) {
                            params.put(name, getValidParam(name));
                        }
                        for(String name : getStepParamInfo().getOptional()) {
                            // TODO: REVIEW: not sure about this currently (getParam(name)?)
                            params.put(name, getValidParam(name));
                        }
                        staticState.stepParams = params;
                    } catch(Exception e) {
                        staticState.stepParams = new HashMap<>(); // prevents multi-failures
                        throw e;
                    }
                }
                return staticState.stepParams;
            }

            @Override
            public boolean isSkippableEffective() {
                if (!isSkippable()) return false;
                
                // this was too limited
                // check if the next step is accessible
                //String nextStep = getStepAfter(getName());
                //if (FINISHED_STEP.equals(nextStep)) return true;
                //else return getStepState(nextStep).isAccessible();
                String nextStep = getNextAccessibleStep();
                return nextStep != null;
            }
            
            @Override
            public String getNextAccessibleStep() {
                String nextStep = getStepAfter(this.getName());
                if (nextStep == null || FINISHED_STEP.equals(nextStep)) return nextStep;
                
                StepState nextState = getStepState(nextStep);
                if (nextState.isAccessible()) {
                    return nextStep;
                } else {
                    if (!isSkippable()) {
                        return null;
                    } else {
                        return nextState.getNextAccessibleStep();
                    }
                }
            }

            private void handleEx(Exception e) {
                throw new IllegalStateException("Setup: Database error while checking setup step '" + getName() + "': " + e.getMessage(), e);
            }
            
            // helpers
            
            protected final StepState getStepBefore() { return getStepState(RequestSetupWorker.this.getStepBefore(getName())); }
            protected final int getStepIndex() { return RequestSetupWorker.this.getStepIndex(getName()); } // TODO: optimize static info
            
            // overridable and default implementations for subclasses
            
            protected abstract Map<String, Object> getStepDataImpl() throws GeneralException;
            
            protected abstract boolean isSkippableImpl() throws GeneralException;
            
            protected boolean isCompletedImpl() throws GeneralException {
                Map<String, Object> stepData = getStepData();
                return Boolean.TRUE.equals(stepData.get("completed"));
            }
            
            protected boolean isCoreCompletedImpl() throws GeneralException {
                Map<String, Object> stepData = getStepData();
                if (Boolean.TRUE.equals(stepData.get("completed"))) return true;
                else return (Boolean.TRUE.equals(stepData.get("coreCompleted")));
            }
            
            // REMOVED: managed to avoid session marks for skipping for now - keeping simple
//            protected boolean isSkippedImpl() throws GeneralException {
//                return Boolean.TRUE.equals(getSkippedSteps().get(getName()));
//            }
   
            protected boolean isAccessibleImpl() throws GeneralException {
                // SIMPLIFIED: all steps will be accessible as soon as their required parameters are present.
                // this means as soon as the first critical steps are done, all the others open up;
                // this way they can be accessed quickly
                //if (getInitialStep().equals(getName())) return true; // first step is always accessible
                //else if (isCompleted()) return isAllRequiredParamsPresent(); // SPECIAL CASE: if it's been filled-in, trust it can always be re-accessed for view or edit (out of order)
                //else if (getStepIndex() <= getStepIndexWithFinished(determineStepAuto(true))) return isAllRequiredParamsPresent(); // if the natural "next" step is us or a step after us, we should be accessible
                //return false;
                return isAllRequiredParamsPresent();
            }
            
            // helpers
            
            protected final boolean isAllRequiredParamsPresent() {
                return isEveryParamValidNonEmpty(getStepParamInfo().getRequired());
            }
            
            protected final Map<String, Object> getStepDataParamsArg() {
                return getCombinedParamsWithEnsureValid(getStepParamInfo().getRequired());
            }
        }
        
        private static final StepParamInfo organizationStepParamInfo =
                StepParamInfo.fromRequiredAndOptional(nameSet(), nameSet(
                        "orgPartyId",
                        "productStoreId" // NOTE: this is not used by the screen, it's merely passed around like a session attr, for now
                        ));
        protected class OrganizationStepState extends CommonStepState {

            public OrganizationStepState(StaticStepState partial) { super(partial); }
            
            @Override public String getName() { return "organization"; }
            @Override protected boolean isSkippableImpl() { return isCoreCompleted(); }
            @Override public StepParamInfo getStepParamInfo() { return organizationStepParamInfo; }
            
            @Override
            protected Map<String, Object> getStepDataImpl() throws GeneralException {
                return SetupDataUtil.getOrganizationStepData(getDelegator(), getDispatcher(), getStepDataParamsArg(), isUseEntityCache());
            }
        }
        
        private static final StepParamInfo userStepParamInfo =
                StepParamInfo.fromRequiredAndOptional(nameSet("orgPartyId", "productStoreId"), nameSet("userPartyId"));
        protected class UserStepState extends CommonStepState {
            public UserStepState(StaticStepState partial) { super(partial); }
            
            @Override public String getName() { return "user"; }
            @Override protected boolean isSkippableImpl() { return true; }
            @Override public StepParamInfo getStepParamInfo() { return userStepParamInfo; }
    
            @Override
            protected Map<String, Object> getStepDataImpl() throws GeneralException {
                return SetupDataUtil.getUserStepData(getDelegator(), getDispatcher(), getStepDataParamsArg(), isUseEntityCache());
            }
        }
        
        private static final StepParamInfo accountingStepParamInfo =
                StepParamInfo.fromRequiredAndOptional(nameSet("orgPartyId"), nameSet()); // "productStoreId", "userPartyId"
        protected class AccountingStepState extends CommonStepState {
            public AccountingStepState(StaticStepState partial) { super(partial); }
            
            @Override public String getName() { return "accounting"; }
            @Override protected boolean isSkippableImpl() { return true; }
            @Override public StepParamInfo getStepParamInfo() { return accountingStepParamInfo; }
            
            @Override
            protected Map<String, Object> getStepDataImpl() throws GeneralException {
                return SetupDataUtil.getAccountingStepData(getDelegator(), getDispatcher(), getStepDataParamsArg(), isUseEntityCache());
            }
        }
        
        private static final StepParamInfo facilityStepParamInfo =
                StepParamInfo.fromRequiredAndOptional(nameSet("orgPartyId"), nameSet("facilityId", "productStoreId"));
        protected class FacilityStepState extends CommonStepState {
            public FacilityStepState(StaticStepState partial) { super(partial); }
            
            @Override public String getName() { return "facility"; }
            @Override protected boolean isSkippableImpl() { return true; }
            @Override public StepParamInfo getStepParamInfo() { return facilityStepParamInfo; }
    
            @Override
            protected Map<String, Object> getStepDataImpl() throws GeneralException {
                return SetupDataUtil.getFacilityStepData(getDelegator(), getDispatcher(), getStepDataParamsArg(), isUseEntityCache());
            }
        }
        
        private static final StepParamInfo catalogStepParamInfo =
                StepParamInfo.fromRequiredAndOptional(nameSet("orgPartyId", "productStoreId"), nameSet());
        protected class CatalogStepState extends CommonStepState {
            public CatalogStepState(StaticStepState partial) { super(partial); }
            
            @Override public String getName() { return "catalog"; }
            @Override protected boolean isSkippableImpl() { return true; }
            @Override public StepParamInfo getStepParamInfo() { return catalogStepParamInfo; }
    
            @Override
            protected Map<String, Object> getStepDataImpl() throws GeneralException {
                return SetupDataUtil.getCatalogStepStateData(getDelegator(), getDispatcher(), getStepDataParamsArg(), isUseEntityCache());
            }
        }
        
        private static final StepParamInfo storeStepParamInfo =
                StepParamInfo.fromRequiredAndOptional(nameSet(
                        "orgPartyId", 
                        "facilityId" // NOTE: 2017-09-27: making this required because too many things require it
                        ), nameSet("productStoreId"));
        protected class StoreStepState extends CommonStepState {
            public StoreStepState(StaticStepState partial) { super(partial); }
            
            @Override public String getName() { return "store"; }
            @Override protected boolean isSkippableImpl() { return isCoreCompleted(); }
            @Override public StepParamInfo getStepParamInfo() { return storeStepParamInfo; }
    
            @Override
            protected Map<String, Object> getStepDataImpl() throws GeneralException {
                return SetupDataUtil.getStoreStepStateData(getDelegator(), getDispatcher(), getStepDataParamsArg(), true, isUseEntityCache());
            }
        }
        
        // merged into store step
//        private static final StepParamInfo websiteStepParamInfo =
//                StepParamInfo.fromRequiredAndOptional(nameSet("orgPartyId", "productStoreId"), nameSet());
//        protected class WebsiteStepState extends CommonStepState {
//            public WebsiteStepState(StaticStepState partial) { super(partial); }
//            
//            @Override public String getName() { return "website"; }
//            @Override protected boolean isSkippableImpl() { return true; }
//            @Override public StepParamInfo getStepParamInfo() { return websiteStepParamInfo; }
//    
//            @Override
//            protected Map<String, Object> getStepDataImpl() throws GeneralException {
//                return SetupDataUtil.getWebsiteStepStateData(getDelegator(), getDispatcher(), getStepDataParamsArg(), isUseEntityCache());
//            }
//        }
        
        // NOTE: this is a "pseudo" step; it typically doesn't get listed as part of the main steps.
        private static final StepParamInfo finishedStepParamInfo =
                StepParamInfo.fromRequiredAndOptional(nameSet("orgPartyId", "productStoreId"), nameSet());
        protected class FinishedStepState extends CommonStepState {
            public FinishedStepState(StaticStepState partial) { super(partial); }
            
            @Override public String getName() { return FINISHED_STEP; }
            @Override protected boolean isSkippableImpl() { return false; }
            @Override public StepParamInfo getStepParamInfo() { return finishedStepParamInfo; }
    
            @Override
            protected Map<String, Object> getStepDataImpl() throws GeneralException {
                Map<String, Object> result = new HashMap<>();
                result.put("completed", true);
                return result;
            }
        }
        
    
        /* 
         * *******************************************
         * Param handling (generic)
         * *******************************************
         */
        
        /**
         * Returns the NON-validated params, minimally-preprocessed.
         */
        protected Map<String, Object> getParams() {
            if (staticWorker.params == null) {
                Map<String, Object> params = UtilHttp.getParameterMap(request);
                params.putAll(UtilHttp.getAttributeMap(request, reqAttrNamesToSkip));
                preprocessParams(params);
                staticWorker.params = params;
            }
            return staticWorker.params;
        }
 
        protected void preprocessParams(Map<String, Object> params) {
            
            // TODO: REVIEW: to avoid certain kinds of crashes, for now
            // do not allow collections on some parameters (they come from UtilHttp.getParameterMap)
            preventCollections(params, ALL_PARAM_ATTR, "Setup: preprocessParams: ");
            
            // DEV NOTE: THESE FALLBACKS INTENTIONALLY CHECK FOR NON-NULL, NOT NON-EMPTY
            // if the param is specified but empty ?orgPartyId= then it will not check for ?partyId=
            // If there are issues, fix them in the screen not here.
            
            // SPECIAL: reads orgPartyId from original params map, but if orgPartyId is null,
            // uses partyId as param instead.
            putFirstNonNull(params, ORGPARTYID_ATTRLIST, params, ORGPARTYID_ATTR);
            
            // SPECIAL: need to recognize orgProductStoreId as a workaround for screen issues.
            // NOTE: we don't use orgProductStoreId internally (for now...)
            putFirstNonNull(params, PRODUCTSTOREID_ATTRLIST, params, PRODUCTSTOREID_ATTR);
        }

        /**
         * Returns the validated params map. No invalid IDs in here, unless they have no {@link ParamValidator}.
         * This map is only populated as-needed by {@link #getValidParam} and {@link #ensureParamsValidated}.
         */
        protected Map<String, Object> getValidParams() {
            if (staticWorker.validParams == null) {
                staticWorker.validParams = new HashMap<>();
            }
            return staticWorker.validParams;
        }
        
        /**
         * Returns params + validParams, and each name in validParamNames receives forced validation
         * (will be null in map if invalid).
         */
        protected Map<String, Object> getCombinedParamsWithEnsureValid(Collection<String> validParamNames) {
            ensureParamsValidated(validParamNames);
            // TODO?: optimize? (large map copies)
            Map<String, Object> combinedParams = new HashMap<>(getParams());
            combinedParams.putAll(getValidParams());
            return combinedParams;
        }
        
        /**
         * Returns NON-validated param; may be straight user input.
         */
        @Override
        public Object getParam(String name) {
            return getParams().get(name);
        }
        
        /**
         * Returns validated user param OR the straight param as-is if no validation is implemented for it.
         */
        @Override
        public Object getValidParam(String name) {
            Map<String, Object> validParams = getValidParams();
            if (!validParams.containsKey(name)) {
                Object value = getParam(name);
                ParamValidator validator = ParamValidator.getValidator(name);
                if (validator != null) {
                    value = validator.getValidatedParam(this, name, value);
                }
                validParams.put(name, value);
                return value;
            }
            return validParams.get(name);
        }
        
        protected void ensureParamsValidated(Collection<String> paramNames) {
            for(String name : paramNames) {
                getValidParam(name);
            }
        }
        
        /**
         * Returns true if all the named parameters are 1) non-empty 2) valid values.
         * FIXME: doesn't handle types, only works with Strings.
         */
        protected boolean isEveryParamValidNonEmpty(Collection<String> paramNames) {
            ensureParamsValidated(paramNames);
            Map<String, Object> validParams = getValidParams();
            for(String name : paramNames) {
                Object val = validParams.get(name);
                if (!(val instanceof String) || UtilValidate.isEmpty((String) val)) {
                    return false;
                }
            }
            return true;
        }
        
        /**
         * This delegates the validation of the params back to the StepStates i.e. SetupDataUtil,
         * so the DB checks only need to run once.
         */
        protected static abstract class ParamValidator implements Serializable {
            private static final Map<String, ParamValidator> paramValidatorMap;
            static {
                Map<String, ParamValidator> map = new HashMap<>();
                
                map.put("orgPartyId", new ParamValidator() {
                    @Override
                    public Object getValidatedParam(RequestSetupWorker setupWorker, String paramName, Object value) {
                        String partyId = null;
                        
                        // NOTE: the value gets implicitly passed here (confusing), so not using the value arg
                        Map<String, Object> storeStepData = setupWorker.getStepState("organization").getStepData();
                        if (storeStepData.get("partyGroup") != null) {
                            partyId = (String) storeStepData.get("orgPartyId");
                        }
                        
                        return partyId;
                    }
                });
                
                map.put("productStoreId", new ParamValidator() {
                    @Override
                    public Object getValidatedParam(RequestSetupWorker setupWorker, String paramName, Object value) {
                        String productStoreId = null;
                        
                        // NOTE: the value gets implicitly passed here (confusing), so not using the value arg
                        Map<String, Object> storeStepData = setupWorker.getStepState("store").getStepData();
                        if (storeStepData.get("productStore") != null) {
                            productStoreId = (String) storeStepData.get("productStoreId");
                        }
                        
                        return productStoreId;
                    }
                });
                
                map.put("facilityId", new ParamValidator() {
                    @Override
                    public Object getValidatedParam(RequestSetupWorker setupWorker, String paramName, Object value) {
                        String facilityId = null;
                        
                        // NOTE: the value gets implicitly passed here (confusing), so not using the value arg
                        Map<String, Object> facilityStepData = setupWorker.getStepState("facility").getStepData();
                        if (facilityStepData.get("facility") != null) {
                            facilityId = (String) facilityStepData.get("facilityId");
                        }
                        
                        return facilityId;
                    }
                });
                
                paramValidatorMap = Collections.unmodifiableMap(map);
            }
 
            public static ParamValidator getValidator(String paramName) {
                return paramValidatorMap.get(paramName);
            }
            
            public abstract Object getValidatedParam(RequestSetupWorker setupWorker, String paramName, Object value);
        }
        

        private GenericValue getStepDataValueIfValid(String step, String valueName, String paramName) {
            String paramVal = (String) getValidParam(paramName);
            if (UtilValidate.isNotEmpty(paramVal)) {
                return getStepDataValue(step, valueName);
            }
            return null;
        }
        
        private GenericValue getStepDataValue(String step, String valueName) {
            Map<String, Object> storeStepData = getStepState(step).getStepData();
            return (GenericValue) storeStepData.get(valueName);
        }
        
        @Override
        public GenericValue getOrgParty() {
            return getStepDataValueIfValid("organization", "party", "orgPartyId");
        }

        @Override
        public GenericValue getOrgPartyGroup() {
            return getStepDataValueIfValid("organization", "partyGroup", "orgPartyId");
        }
        
        @Override
        public GenericValue getProductStore() {
            return getStepDataValueIfValid("store", "productStore", "productStoreId");
        }
        
        /* 
         * *******************************************
         * Local Helpers
         * *******************************************
         */
        
        private Delegator getDelegator() {
            Delegator delegator = this.delegator;
            if (delegator == null) {
                delegator = (Delegator) request.getAttribute("delegator");
                this.delegator = delegator;
            }
            return delegator;
        }
        
        protected LocalDispatcher getDispatcher() {
            LocalDispatcher dispatcher = this.dispatcher;
            if (dispatcher == null) {
                dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
                this.dispatcher = dispatcher;
            }
            return dispatcher;
        }
        
        protected HttpServletRequest getRequest() {
            return request;
        }
        
        protected HttpSession getSession() {
            return request.getSession();
        }

        protected boolean isUseEntityCache() {
            return false;
        }
    }
    
    /* 
     * *******************************************
     * Generic helpers
     * *******************************************
     */
    
    private static String getNonNull(String value) {
        return (value != null) ? value : "";
    }
    
    private static String getNonEmptyOrNull(String value) {
        if (value == null) return null;
        else return (value.length() > 0) ? value : null;
    }
    
    private static Set<String> nameSet(String... values) {
        return StepParamInfo.nameSet(values);
    }
    
    private static boolean strEquals(String first, String second) {
        if (first != null) return first.equals(second);
        else return second == null;
    }
    
    private static boolean containsOneOf(Map<String, Object> params, Iterable<String> names) {
        for(String name : names) {
            if (params.containsKey(name)) return true;
        }
        return false;
    }
    
    private static String getFirstNonNull(Map<String, Object> params, Iterable<String> names) {
        for(String name : names) {
            Object value = params.get(name);
            if (value != null) return null;
        }
        return null;
    }
    
    private static Object putFirstNonNull(Map<String, Object> params, Iterable<String> names, Map<String, Object> destMap, String destName) {
        for(String name : names) {
            Object value = params.get(name);
            if (value != null) {
                destMap.put((destName != null) ? destName : name, value);
                return value;
            }
        }
        return null;
    }
    
    private static void preventCollections(Map<String, Object> params, String... paramNames) {
        preventCollections(params, Arrays.asList(paramNames), null);
    }

    private static void preventCollections(Map<String, Object> params, Collection<String> paramNames) {
        preventCollections(params, paramNames, null);
    }
    
    private static void preventCollections(Map<String, Object> params, Collection<String> paramNames, String logPrefix) {
        for(String name : paramNames) {
            if (params.get(name) instanceof Collection) {
                Iterator<?> it = UtilGenerics.checkCollection(params.get(name)).iterator();
                if (logPrefix != null) {
                    Debug.logWarning(logPrefix+"Request parameter/attribute '" + name 
                            + "' was found to be a collection (multiple request parameters with same name?); using first value only", module);
                }
                Object firstValue = it.hasNext() ? it.next() : null;
                params.put(name, firstValue);
            }
        }
    }
}
