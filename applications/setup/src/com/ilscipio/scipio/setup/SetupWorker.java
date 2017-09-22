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

    protected static final String SKIPPEDSTEPS_ATTR = "scpSkippedSetupStates";

    /**
     * Name of req attribute that holds a StaticSetupWorker for caching.
     */
    protected static final String STATIC_WORKER_ATTR =  "scpStaticSetupWorker";
    
    /**
     * Full list of visible steps.
     * NOTE: see Step States further below for specific implementations.
     */
    private static final List<String> stepsList = UtilMisc.unmodifiableArrayList(
            "organization", "store", "user", "accounting", "facility", "catalog", "website"
    );
    private static final Set<String> stepsSet = Collections.unmodifiableSet(new HashSet<String>(stepsList));
    
    public static final String FINISHED_STEP = "finished";
    public static final String ERROR_STEP = "error";
    
    private static final Set<String> specialStepValues = UtilMisc.unmodifiableHashSet(
            FINISHED_STEP, ERROR_STEP
    );
    
    private static final Set<String> allStepValues;
    static {
        Set<String> s = new HashSet<>(stepsSet);
        s.addAll(specialStepValues);
        allStepValues = Collections.unmodifiableSet(s);
    }
    
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
    public Set<String> getAllStepValues() { return allStepValues; }
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
        public abstract String getName();
        public abstract boolean isSkippable();
        public abstract boolean isCompleted();
        public abstract boolean isAccessible();
        public abstract boolean isSkipped();
        public abstract Map<String, Object> getStepData();
        /**
         * The params that should be passed to this step to access it.
         */
        public abstract Map<String, Object> getStepParams();
        public abstract StepParamInfo getStepParamInfo();
        
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
            map.put("stepData", getStepData());
            map.put("skippable", isSkippable());
            map.put("completed", isCompleted());
            map.put("skipped", isSkipped());
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
     * Helper to read current product store; only non-null if valid.
     */
    public abstract GenericValue getProductStore();
    
    /**
     * Returns last submitted setup step (rather than "current"/"next").
     */
    public abstract String getSubmittedStep();
    
    /**
     * WARN: must be verified.
     */
    public abstract String getRequestedStep();
    
    public abstract String getEffectiveStep();
    
    /**
     * WARN: Call before everything else. Triggers cache clear.
     */
    public abstract void setEffectiveStep(String effectiveStep);
    
    /**
     * Returns the "current"/"next" step.
     * In all cases, if the EFF_SETUP_STEP_ATTR is set, everything else is ignored.
     * <p>
     * NOTE: Upon review, screens don't need to call this (only events); they set a context var and just
     * need to validate it with {@link #isStepEffectiveSafe}.
     * <p>
     * @param useRequested if TRUE check requested and error if invalid; if FALSE ignore requested; 
     *                     if null check requested but fallback if invalid
     * @param updateEffective if TRUE always update effective even if exception (sets "error"); if FALSE never update;
     *                        if null only update if non-exception
     * @see #getSubmittedStep()
     */
    public abstract String getStep(Boolean useRequested, Boolean updateEffective);
    
    /**
     * Full-featured getSetupStep call, same as <code>getSetupStep(Boolean.TRUE, Boolean.TRUE)</code>.
     * <p>
     * NOTE: Upon review, screens don't need to call this (only events); they set a context var and just
     * need to validate it with {@link #isSetupStepEffectiveSafe}.
     */
    public abstract String getStep();
    
    public abstract String determineStep(Boolean useRequested);
    
    public abstract String determineStepAuto(boolean useCache);
    
    /**
     * WARN: Call before everything else. Triggers cache clear.
     */
    public abstract void markStepSkipped(String step) throws IllegalArgumentException, IllegalStateException;

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
        public String getStep(Boolean useRequested, Boolean updateEffective) {
            throw new UnsupportedOperationException(); // TODO?
        }
        @Override
        public String getStep() {
            throw new UnsupportedOperationException(); // TODO?
        }
        @Override
        public String determineStep(Boolean useRequested) {
            throw new UnsupportedOperationException(); // TODO?
        }
        @Override
        public String determineStepAuto(boolean useCache) {
            throw new UnsupportedOperationException(); // TODO?
        }
        @Override
        public void markStepSkipped(String step) throws IllegalArgumentException {
            throw new UnsupportedOperationException(); // TODO?
        }
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
            for(String step : getSteps()) {
                stateMap.put(step, Collections.unmodifiableMap(getStepState(step).toMap()));
            }
            return Collections.unmodifiableMap(stateMap);
        }
        
        protected static class StaticStepState extends StepState { // WARN: this one MUST be static
            protected String name;
            // NOTE: are boxed types instead of primitives due to reuse for caching
            protected Boolean skippable;
            protected Boolean completed;
            protected Boolean skipped;
            protected Boolean accessible;
            protected Map<String, Object> stepData;
            protected Map<String, Object> stepParams;
            protected boolean resolved; // FIXME: handle this flag better - it's only set in subset of possible cases, for opt
            
            protected StaticStepState() { ; }
            
            public StaticStepState(StepState other) {
                this.name = other.getName();
                this.skippable = other.isSkippable();
                this.completed = other.isCompleted();
                this.skipped = other.isSkipped();
                this.accessible = other.isAccessible();
                this.stepData = other.getStepData();
                this.stepParams = other.getStepParams();
                this.resolved = true; // the calls above enforce this
            }
            
            public StaticStepState(StaticStepState partial, StepState other) {
                this.name = partial.name != null ? partial.name : other.getName();
                this.skippable = partial.skippable != null ? partial.skippable : other.isSkippable();
                this.completed = partial.completed != null ? partial.completed : other.isCompleted();
                this.skipped = partial.skipped != null ? partial.skipped : other.isSkipped();
                this.accessible = partial.accessible != null ? partial.accessible : other.isAccessible();
                this.stepData = partial.stepData != null ? partial.stepData : other.getStepData();
                this.stepParams = partial.stepParams != null ? partial.stepParams : other.getStepParams();
                this.resolved = true; // the calls above enforce this
            }
            
            protected StaticStepState(StaticStepState partial, boolean partialIsComplete) {
                this.name = partial.name;
                this.skippable = partial.skippable;
                this.completed = partial.completed;
                this.skipped = partial.skipped;
                this.accessible = partial.accessible;
                this.stepData = partial.stepData;
                this.stepParams = partial.stepParams;
                this.resolved = partial.resolved;
            }

            @Override public String getName() { return name; }
            @Override public boolean isSkippable() { return skippable; }
            @Override public boolean isCompleted() { return completed; }
            @Override public boolean isSkipped() { return skipped; }
            @Override public boolean isAccessible() { return accessible; }
            @Override public Map<String, Object> getStepData() { return stepData; }
            @Override public Map<String, Object> getStepParams() { return stepParams; }
            
            protected boolean isResolved() { return resolved; }
            protected void markResolved() { resolved = true; }

            @Override
            public StepParamInfo getStepParamInfo() {
                throw new UnsupportedOperationException(); // not yet needed
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
                // SPECIAL: WE MUST CLEAR ALL CACHE AFTER THIS
                this.clearCached();
            }
        }
        
        @Override
        public String getStep(Boolean useRequested, Boolean updateEffective) {
            String step = getEffectiveStep();
            if (step == null) {
                try {
                    step = determineStep(useRequested);
                    if (!Boolean.FALSE.equals(updateEffective)) setEffectiveStep(step);
                } catch(Exception e) {
                    if (Boolean.TRUE.equals(updateEffective)) setEffectiveStep(ERROR_STEP);
                    throw e;
                }
            }
            return step;
        }
        
        @Override
        public String getStep() {
            return getStep(Boolean.TRUE, Boolean.TRUE);
        }
        
        @Override
        public String determineStep(Boolean useRequested) {
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
            return determineStepAuto(true);
        }
        
        @Override
        public String determineStepAuto(boolean useCache) {
            String step = staticWorker.autoDetStep;
            if (step == null) {
                for(StepState state : getStepStatesInternal()) {
                    if (!state.isCompleted() && !state.isSkipped()) {
                        step = state.getName();
                        break;
                    }
                }
                if (step == null) step = FINISHED_STEP;
                staticWorker.autoDetStep = step;
            }
            return step;
        }
        
        private Map<String, Boolean> getSkippedSteps() {
            Map<String, Boolean> skippedSteps = UtilGenerics.checkMap(request.getAttribute(SKIPPEDSTEPS_ATTR));
            if (skippedSteps == null) {
                // NOTE: we only ever read once from session then use request thereafter
                skippedSteps = UtilGenerics.checkMap(getSession().getAttribute(SKIPPEDSTEPS_ATTR));
                if (skippedSteps == null) skippedSteps = Collections.emptyMap();
                // set in request to avoid sync issues
                request.setAttribute(SKIPPEDSTEPS_ATTR, skippedSteps);
            }
            return skippedSteps;
        }
        
        /**
         * Marks step skipped in session regardless of whether skippable(!).
         */
        protected void markStepSkippedForce(String step) throws IllegalArgumentException {     
            Map<String, Boolean> skippedSteps = getSkippedSteps();
            if (!Boolean.TRUE.equals(skippedSteps.get(step))) {
                try {
                    skippedSteps = new HashMap<>(skippedSteps);
                    skippedSteps.put(step, Boolean.TRUE);
                    skippedSteps = Collections.unmodifiableMap(skippedSteps); // immutable/thread-safe
                    request.setAttribute(SKIPPEDSTEPS_ATTR, skippedSteps);
                    getSession().setAttribute(SKIPPEDSTEPS_ATTR, skippedSteps);
                } finally {
                    // SPECIAL: WE MUST CLEAR ALL CACHE AFTER THIS
                    this.clearCached();
                }
            }
        }
        
        @Override
        public void markStepSkipped(String step) throws IllegalArgumentException, IllegalStateException {
            // VERIFY
            if (!getStepState(step).isSkippable()) throw new IllegalStateException("step is not skippable: " + step);
            if (!isStepAllowed(getStepAfter(step))) throw new IllegalStateException("step cannot be skipped (at this point): " + step);

            markStepSkippedForce(step);
        }
    
        @Override
        public boolean isStepAllowed(String requestedStep) throws IllegalArgumentException {
            if (ERROR_STEP.equals(requestedStep)) return true;
            if (!getAllStepValues().contains(requestedStep)) {
                throw new IllegalArgumentException("invalid setup step requested: " + requestedStep);
            }
            for(StepState state : getStepStatesInternal()) {
                if (state.getName().equals(requestedStep)) {
                    return true;
                }
                // NOTE: I switched out state.isSkippable() with isSkipped for now,
                // but might want it back later...
                if (!state.isSkipped() && !state.isCompleted()) { // 
                    return false;
                }
            }
            return FINISHED_STEP.equals(requestedStep);
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
                Debug.logError("Setup: Error determining setup step: " + e.getMessage(), module);
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
            if (getStepStateMapPriv().size() < getSteps().size()) {
                for(String step : getSteps()) {
                    getStepState(step);
                }
            }
            return getStepStateMapPriv();
        }
        
        private Collection<StepState> getStepStatesInternal() {
            Map<String, StepState> stepStateMap = getStepStateMapLoaded();
            List<StepState> values = new ArrayList<>();
            for(String step : getSteps()) {
                values.add(stepStateMap.get(step));
            }
            return values;
        }
        
        @Override
        public Map<String, StepState> getStepStateMap() {
            return Collections.unmodifiableMap(getStepStateMapLoaded());
        }

        @Override
        public Map<String, Map<String, Object>> getStepStatePrimitiveMap() throws IllegalArgumentException {
            Map<String, Map<String, Object>> stateMap = new HashMap<>();
            for(String step : getSteps()) {
                stateMap.put(step, Collections.unmodifiableMap(getStepState(step).toMap()));
            }
            return Collections.unmodifiableMap(stateMap);
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
            m.put("website", WebsiteStepState.class);
            
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
                isSkippable();
                isCompleted();
                isSkipped();
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
            public boolean isSkipped() { 
                if (staticState.skipped == null) {
                    try {
                        staticState.skipped = isSkippedImpl();
                    } catch (GeneralException e) {
                        handleEx(e);
                        staticState.skipped = false; // prevents multi-failures
                    }
                } 
                return staticState.skipped;
            }
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
                Map<String, Object> params = new HashMap<>();
                for(String name : getStepParamInfo().getRequired()) {
                    params.put(name, getValidParam(name));
                }
                for(String name : getStepParamInfo().getOptional()) {
                    // TODO: REVIEW: not sure about this currently (getParam(name)?)
                    params.put(name, getValidParam(name));
                }
                return params;
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
            
            protected boolean isSkippedImpl() throws GeneralException {
                return Boolean.TRUE.equals(getSkippedSteps().get(getName()));
            }
   
            protected boolean isAccessibleImpl() throws GeneralException {
                if (getInitialStep().equals(getName())) return true; // first step is always accessible
                else if (isCompleted()) return isAllRequiredParamsPresent(); // SPECIAL CASE: if it's been filled-in, trust it can always be re-accessed for view or edit (out of order)
                else if (getStepIndex() <= getStepIndexWithFinished(determineStepAuto(true))) return isAllRequiredParamsPresent(); // if the natural "next" step is us or a step after us, we should be accessible
                return false;
            }
            
            protected boolean isAllRequiredParamsPresent() {
                return isEveryValidParamNonEmpty(getStepParamInfo().getRequired());
            }
            
            protected Map<String, Object> getStepDataParams() {
                return getCombinedParamsWithEnsureValid(getStepParamInfo().getRequired());
            }
        }
        
        private static final StepParamInfo organizationStepParamInfo =
                StepParamInfo.fromRequiredAndOptional(nameSet(), nameSet("orgPartyId"));
        protected class OrganizationStepState extends CommonStepState {

            public OrganizationStepState(StaticStepState partial) { super(partial); }
            
            @Override public String getName() { return "organization"; }
            @Override protected boolean isSkippableImpl() { return true; }
            @Override public StepParamInfo getStepParamInfo() { return organizationStepParamInfo; }
            
            @Override
            protected Map<String, Object> getStepDataImpl() throws GeneralException {
                return SetupDataUtil.getOrganizationStepData(getDelegator(), getDispatcher(), getStepDataParams(), isUseEntityCache());
            }
        }
        
        private static final StepParamInfo userStepParamInfo =
                StepParamInfo.fromRequiredAndOptional(nameSet("orgPartyId", "productStoreId"), nameSet());
        protected class UserStepState extends CommonStepState {
            public UserStepState(StaticStepState partial) { super(partial); }
            
            @Override public String getName() { return "user"; }
            @Override protected boolean isSkippableImpl() { return true; }
            @Override public StepParamInfo getStepParamInfo() { return userStepParamInfo; }
    
            @Override
            protected Map<String, Object> getStepDataImpl() throws GeneralException {
                return SetupDataUtil.getUserStepData(getDelegator(), getDispatcher(), getStepDataParams(), isUseEntityCache());
            }
        }
        
        private static final StepParamInfo accountingStepParamInfo =
                StepParamInfo.fromRequiredAndOptional(nameSet("orgPartyId", "productStoreId"), nameSet());
        protected class AccountingStepState extends CommonStepState {
            public AccountingStepState(StaticStepState partial) { super(partial); }
            
            @Override public String getName() { return "accounting"; }
            @Override protected boolean isSkippableImpl() { return true; }
            @Override public StepParamInfo getStepParamInfo() { return accountingStepParamInfo; }
            
            @Override
            protected Map<String, Object> getStepDataImpl() throws GeneralException {
                return SetupDataUtil.getAccountingStepData(getDelegator(), getDispatcher(), getStepDataParams(), isUseEntityCache());
            }
        }
        
        private static final StepParamInfo facilityStepParamInfo =
                StepParamInfo.fromRequiredAndOptional(nameSet("orgPartyId", "productStoreId"), nameSet());
        protected class FacilityStepState extends CommonStepState {
            public FacilityStepState(StaticStepState partial) { super(partial); }
            
            @Override public String getName() { return "facility"; }
            @Override protected boolean isSkippableImpl() { return true; }
            @Override public StepParamInfo getStepParamInfo() { return facilityStepParamInfo; }
    
            @Override
            protected Map<String, Object> getStepDataImpl() throws GeneralException {
                return SetupDataUtil.getFacilityStepData(getDelegator(), getDispatcher(), getStepDataParams(), isUseEntityCache());
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
                return SetupDataUtil.getCatalogStepStateData(getDelegator(), getDispatcher(), getStepDataParams(), isUseEntityCache());
            }
        }
        
        private static final StepParamInfo storeStepParamInfo =
                StepParamInfo.fromRequiredAndOptional(nameSet("orgPartyId"), nameSet("productStoreId"));
        protected class StoreStepState extends CommonStepState {
            public StoreStepState(StaticStepState partial) { super(partial); }
            
            @Override public String getName() { return "store"; }
            @Override protected boolean isSkippableImpl() { return true; }
            @Override public StepParamInfo getStepParamInfo() { return storeStepParamInfo; }
    
            @Override
            protected Map<String, Object> getStepDataImpl() throws GeneralException {
                return SetupDataUtil.getStoreStepStateData(getDelegator(), getDispatcher(), getStepDataParams(), isUseEntityCache());
            }
        }
        
        private static final StepParamInfo websiteStepParamInfo =
                StepParamInfo.fromRequiredAndOptional(nameSet("orgPartyId", "productStoreId"), nameSet());
        protected class WebsiteStepState extends CommonStepState {
            public WebsiteStepState(StaticStepState partial) { super(partial); }
            
            @Override public String getName() { return "website"; }
            @Override protected boolean isSkippableImpl() { return true; }
            @Override public StepParamInfo getStepParamInfo() { return websiteStepParamInfo; }
    
            @Override
            protected Map<String, Object> getStepDataImpl() throws GeneralException {
                return SetupDataUtil.getWebsiteStepStateData(getDelegator(), getDispatcher(), getStepDataParams(), isUseEntityCache());
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
            preventCollections(params, "partyId", "orgPartyId", "productStoreId");
            
            // SPECIAL: set partyId as orgPartyId if applicable, to support both ?orgPartyId= and ?partyId=
            params.put("orgPartyId", getOrgPartyIdParam(params));
        }
        
        protected static void preventCollections(Map<String, Object> params, String... paramNames) {
            preventCollections(params, Arrays.asList(paramNames));
        }

        protected static void preventCollections(Map<String, Object> params, Collection<String> paramNames) {
            for(String name : paramNames) {
                if (params.get(name) instanceof Collection) {
                    Iterator<?> it = UtilGenerics.checkCollection(params.get(name)).iterator();
                    Debug.logWarning("Setup: preprocessParams: request parameter/attribute '" + name 
                            + "' was found to be a collection (multiple request parameters with same name?); using first value only", module);
                    Object firstValue = it.hasNext() ? it.next() : null;
                    params.put(name, firstValue);
                }
            }
        }
        
        /**
         * SPECIAL: reads orgPartyId from original params map, but if orgPartyId is null,
         * uses partyId as param instead.
         */
        protected static String getOrgPartyIdParam(Map<String, Object> params) {
            String orgPartyId = (String) params.get("orgPartyId");
            if (orgPartyId == null) orgPartyId = (String) params.get("partyId");
            return orgPartyId;
        }
        
        /**
         * SPECIAL: need to recognize orgProductStoreId as a workaround for screen issues.
         * NOTE: we don't use orgProductStoreId internally (for now...)
         * If both are specified, they should always have the same value.
         */
        protected static String getOrgProductStoreIdParam(Map<String, Object> params) {
            String orgProductStoreId = (String) params.get("orgProductStoreId");
            String productStoreId = (String) params.get("productStoreId");
            
            if (UtilValidate.isNotEmpty(orgProductStoreId) && UtilValidate.isNotEmpty(productStoreId) &&
                    !orgProductStoreId.equals(productStoreId)) {
                Debug.logError("Setup: preprocessParams: the values of orgProductStoreId (" + orgProductStoreId 
                        + ") and productStoreId (" + productStoreId 
                        + ") parameters/attributes are different; they are expected to be the same", module);
            }

            return orgProductStoreId != null ? orgProductStoreId : productStoreId;
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
         * FIXME: doesn't handle types, only works with Strings.
         */
        protected boolean isEveryValidParamNonEmpty(Collection<String> paramNames) {
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
                        if (Boolean.TRUE.equals(storeStepData.get("partyValid"))) {
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
                        if (Boolean.TRUE.equals(storeStepData.get("storeValid"))) {
                            productStoreId = (String) storeStepData.get("productStoreId");
                        }
                        
                        return productStoreId;
                    }
                });
                
                paramValidatorMap = Collections.unmodifiableMap(map);
            }
 
            public static ParamValidator getValidator(String paramName) {
                return paramValidatorMap.get(paramName);
            }
            
            public abstract Object getValidatedParam(RequestSetupWorker setupWorker, String paramName, Object value);
        }
        
        @Override
        public GenericValue getProductStore() {
            String productStoreId = getProductStoreId();
            if (UtilValidate.isNotEmpty(productStoreId)) {
                Map<String, Object> storeStepData = getStepState("store").getStepData();
                return (GenericValue) storeStepData.get("productStore");
            }
            return null;
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
    
    protected static String getNonNull(String value) {
        return (value != null) ? value : "";
    }
    
    protected static String getNonEmptyOrNull(String value) {
        if (value == null) return null;
        else return (value.length() > 0) ? value : null;
    }
    
    protected static Set<String> nameSet(String... values) {
        return StepParamInfo.nameSet(values);
    }
}
