package com.ilscipio.scipio.setup;

import java.io.Serializable;
import java.lang.reflect.Constructor;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;

/**
 * SCIPIO: Setup worker.
 * NOT thread-safe.
 * WARN: due to caching, DB and other exceptions can come from basically any method.
 */
@SuppressWarnings("serial")
public class SetupWorker implements Serializable {

    public static final String module = SetupWorker.class.getName();
    
    /**
     * "Next" setup step that can be requested via req params or attributes (is verified for valid).
     */
    public static final String REQ_STEP_ATTR = "scpSetupStep";
    
    /**
     * The step we are submitting form for ("last" step).
     */
    public static final String SUBMIT_STEP_ATTR = "scpSubmitSetupStep";
    
    /**
     * Effective setup step, cached in request after event or first access.
     */
    public static final String EFF_STEP_ATTR = "scpEffSetupStep";
    
    public static final String EFF_STEPSTATES_ATTR = "scpEffSetupStepStates";

    public static final String SKIPPEDSTEPS_ATTR = "scpSkippedSetupStates";

    /**
     * Full list of visible steps.
     * NOTE: see Step States further below for specific implementations.
     */
    private static final List<String> stepsList = UtilMisc.unmodifiableArrayList(
            "organization", "user", "accounting", "facility", "catalog", "website"
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
    

    private final HttpServletRequest request;
    private transient Delegator delegator;
    private Map<String, StepState> stepStateMap = new HashMap<>();
    private String autoDetStep = null;
    
    /* 
     * *******************************************
     * Constructors
     * *******************************************
     */
    
    protected SetupWorker(HttpServletRequest request) {
        this.request = request;
    }
    
    public static SetupWorker getWorker(HttpServletRequest request) {
        return new SetupWorker(request);
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
    
    /* 
     * *******************************************
     * Stateful queries
     * *******************************************
     */
    
    /**
     * Returns last submitted setup step (rather than "current"/"next").
     */
    public String getSubmittedStep() {
        String lastStep = (String) request.getAttribute(SUBMIT_STEP_ATTR);
        if (lastStep == null) {
            lastStep = request.getParameter(SUBMIT_STEP_ATTR);
        }
        return lastStep;
    }
    
    /**
     * WARN: must be verified.
     */
    public String getRequestedStep() {
        String step = (String) request.getAttribute(REQ_STEP_ATTR);
        if (step == null) {
            step = request.getParameter(REQ_STEP_ATTR);
        }
        return step;
    }
    
    public String getEffectiveStep() {
        return (String) request.getAttribute(EFF_STEP_ATTR);
    }
    
    public void setEffectiveStep(String effectiveStep) {
        request.setAttribute(EFF_STEP_ATTR, effectiveStep);
    }
    
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
    
    /**
     * Full-featured getSetupStep call, same as <code>getSetupStep(Boolean.TRUE, Boolean.TRUE)</code>.
     * <p>
     * NOTE: Upon review, screens don't need to call this (only events); they set a context var and just
     * need to validate it with {@link #isSetupStepEffectiveSafe}.
     */
    public String getStep() {
        return getStep(Boolean.TRUE, Boolean.TRUE);
    }
    
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
    
    public String determineStepAuto(boolean useCache) {
        String step = this.autoDetStep;
        if (step == null) {
            for(StepState state : getStepStatesInternal(true)) {
                if (!state.isCompleted() && !state.isSkipped()) {
                    step = state.getName();
                    break;
                }
            }
            if (step == null) step = FINISHED_STEP;
            this.autoDetStep = step;
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
            skippedSteps = new HashMap<>(skippedSteps);
            skippedSteps.put(step, Boolean.TRUE);
            skippedSteps = Collections.unmodifiableMap(skippedSteps); // immutable/thread-safe
            request.setAttribute(SKIPPEDSTEPS_ATTR, skippedSteps);
            getSession().setAttribute(SKIPPEDSTEPS_ATTR, skippedSteps);
        }
    }
    
    /**
     * WARN: call before everything else
     */
    public void markStepSkipped(String step) throws IllegalArgumentException {
        // VERIFY
        if (!isStepAllowed(getStepAfter(step))) throw new IllegalArgumentException("step cannot be skipped (at this point): " + step);
    }

    public boolean isStepAllowed(String requestedStep) throws IllegalArgumentException {
        if (ERROR_STEP.equals(requestedStep)) return true;
        if (!getAllStepValues().contains(requestedStep)) {
            throw new IllegalArgumentException("invalid setup step requested: " + requestedStep);
        }
        for(StepState state : getStepStatesInternal(true)) {
            if (state.getName().equals(requestedStep)) {
                return true;
            }
            if (!state.isSkippable() && !state.isCompleted()) {
                return false;
            }
        }
        return FINISHED_STEP.equals(requestedStep);
    }
    
    /**
     * If the effective is set, checks if the step matches it; if effective is not set, checks if the step is allowed.
     * FOR USE IN SCREENS to prevent bypassing the wizard.
     */
    public boolean isStepEffectiveSafe(String step) {
        if (UtilValidate.isEmpty(step)) return true;
        try {
            String effStep = getEffectiveStep();
            if (effStep != null) { 
                // if an eff step was set, we have to match it exactly
                return effStep.equals(step);
            } else { 
                // if there was no eff step (something was bypassed), we could just return false, 
                // but can just check it this way instead... don't think can hurt
                return isStepAllowed(step);
            }
        } catch (Exception e) {
            Debug.logError("Setup: Error determining setup step: " + e.getMessage(), module);
            return false;
        }
    }
    
    /* 
     * *******************************************
     * Step state interfaces
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
        
        public void toMap(Map<String, Object> map) {
            map.put("skippable", isSkippable());
            map.put("completed", isCompleted());
            map.put("skipped", isSkipped());
            map.put("accessible", isAccessible());
            map.put("disabled", !isAccessible());
        }
        public Map<String, Object> toMap() {
            Map<String, Object> map = new HashMap<>();
            toMap(map);
            return map;
        }
    }
    
    private static class ResolvedStepState extends StepState { // WARN: this one MUST be static
        protected String name;
        // NOTE: these are Boolean instead of boolean due to reuse by LazyResolvedStepState
        // but if the non-default constructors below are used, they will never be null (internal quirk).
        protected Boolean skippable;
        protected Boolean completed;
        protected Boolean skipped;
        protected Boolean accessible;
        
        protected ResolvedStepState() { ; }
        
        public ResolvedStepState(StepState other) {
            this.name = other.getName();
            this.skippable = other.isSkippable();
            this.completed = other.isCompleted();
            this.skipped = other.isSkipped();
            this.accessible = other.isAccessible();
        }
        
        public ResolvedStepState(ResolvedStepState partial, StepState other) {
            this.name = partial.name != null ? partial.name : other.getName();
            this.skippable = partial.skippable != null ? partial.skippable : other.isSkippable();
            this.completed = partial.completed != null ? partial.completed : other.isCompleted();
            this.skipped = partial.skipped != null ? partial.skipped : other.isSkipped();
            this.accessible = partial.accessible != null ? partial.accessible : other.isAccessible();
        }
        
        protected ResolvedStepState(ResolvedStepState partial, boolean complete) {
            // assumes complete==false
            this.name = partial.name;
            this.skippable = partial.skippable;
            this.completed = partial.completed;
            this.skipped = partial.skipped;
            this.accessible = partial.accessible;
        }

        @Override public String getName() { return name; }
        @Override public boolean isSkippable() { return skippable; }
        @Override public boolean isCompleted() { return completed; }
        @Override public boolean isSkipped() { return skipped; }
        @Override public boolean isAccessible() { return accessible; }
    }
    
    private abstract static class LazyResolvedStepState extends ResolvedStepState {
        public LazyResolvedStepState() { ; }
        public LazyResolvedStepState(ResolvedStepState partial) {
            this.skippable = partial.skippable;
            this.completed = partial.completed;
            this.skipped = partial.skipped;
            this.accessible = partial.accessible;
        }

        protected ResolvedStepState toPartialResolved() {
            return new ResolvedStepState(this, false);
        }
        
        @Override public boolean isSkippable() { if (skippable == null) { skippable = isSkippableImpl(); } return skippable; }
        @Override public boolean isCompleted() { if (completed == null) { completed = isCompletedImpl(); } return completed; }
        @Override public boolean isSkipped() { if (skipped == null) { skipped = isSkippedImpl(); } return skipped; }
        @Override public boolean isAccessible() { if (accessible == null) { accessible = isAccessibleImpl(); } return accessible; }
        
        protected abstract boolean isSkippableImpl();
        protected abstract boolean isCompletedImpl();
        protected abstract boolean isSkippedImpl();
        protected abstract boolean isAccessibleImpl();
    }
    
    public void checkRequestStepStateMap() {
        if (stepStateMap.size() == 0) {
            forceUseRequestStepStateMap();
        }
    }
    
    public void forceUseRequestStepStateMap() {
        stepStateMap.clear();
        Map<String, ResolvedStepState> reqMap = UtilGenerics.checkMap(request.getAttribute(EFF_STEPSTATES_ATTR));
        if (reqMap != null) {
            // COPY THE ENTRIES: the classes have missing refs so we must duplicate them (FIXME: couldn't think of something better)
            // to give them new "other" references
            for(Map.Entry<String, ResolvedStepState> entry : reqMap.entrySet()) {
                stepStateMap.put(entry.getKey(), getNewStepState(entry.getKey(), entry.getValue()));
            }
        }
    }
    
    private StepState getNewStepState(String step) {
        return getNewStepState(step, null);
    }
    
    private StepState getNewStepState(String step, ResolvedStepState fromPartial) {
        Class<?> cls = stepStateClsMap.get(step);
        if (cls == null) throw new IllegalArgumentException("invalid setup step: " + step);
        Constructor<?> ctor;
        try {
            if (fromPartial != null) {
                ctor = cls.getDeclaredConstructor(SetupWorker.class, ResolvedStepState.class);
                return (StepState) ctor.newInstance(this, fromPartial);
            } else {
                ctor = cls.getDeclaredConstructor(SetupWorker.class);
                return (StepState) ctor.newInstance(this);
            }
        } catch (Exception e) {
            Debug.logError(e, module);
            throw new IllegalArgumentException("invalid setup step: " + step);
        }
    }
    
    public StepState getStepState(String step, boolean lazyResolve) throws IllegalArgumentException {
        checkRequestStepStateMap();
        StepState stepState = stepStateMap.get(step);
        if (stepState == null) {
            stepState = getNewStepState(step);
            if (!lazyResolve) { // by default the classes behave as lazyResolve==true
                stepState = new ResolvedStepState(stepState);
            }
            stepStateMap.put(step, stepState);
        } else {
            if (!lazyResolve && !(stepState instanceof ResolvedStepState)) {
                stepState = new ResolvedStepState(stepState);
                stepStateMap.put(step, stepState);
            }
        }
        return stepState;
    }
    
    public StepState getStepState(String step) throws IllegalArgumentException {
        return getStepState(step, true);
    }
    
    private Map<String, StepState> getStepStateMapLoaded(boolean lazyResolve) {
        checkRequestStepStateMap();
        if (stepStateMap.size() < getSteps().size()) {
            for(String step : getSteps()) {
                getStepState(step, lazyResolve);
            }
        }
        return stepStateMap;
    }
    
    private Collection<StepState> getStepStatesInternal(boolean lazyResolve) {
        Map<String, StepState> stepStateMap = getStepStateMapLoaded(lazyResolve);
        List<StepState> values = new ArrayList<>();
        for(String step : getSteps()) {
            values.add(stepStateMap.get(step));
        }
        return values;
    }
    
    public Map<String, StepState> getStepStateMap(boolean lazyResolve) {
        return Collections.unmodifiableMap(getStepStateMapLoaded(lazyResolve));
    }
    
    /**
     * Optimization: Event can call this so screens don't redo the lookups 2-3 times.
     */
    public void saveStepStatesToRequest(boolean lazyResolve) {
        checkRequestStepStateMap();
        Map<String, ResolvedStepState> res = new HashMap<>();
        for(Map.Entry<String, StepState> entry : stepStateMap.entrySet()) {
            ResolvedStepState state = (ResolvedStepState) entry.getValue();
            if (state instanceof CommonStepState) {
                state = ((CommonStepState) state).toPartialResolved();
            }
            res.put(entry.getKey(), state);
        }
        request.setAttribute(EFF_STEPSTATES_ATTR, Collections.unmodifiableMap(res));
    }
    

    /**
     * Returns a map of step names to submaps with keys (all pre-resolved values) and caches in request:
     * accessible (Boolean), skippable (Boolean), skipped (Boolean), completed (Boolean)
     * NOTE: Only for use in screens after all events processed.
     */
    public Map<String, Map<String, Object>> getStepStatePrimitiveMap() throws IllegalArgumentException {
        Map<String, Map<String, Object>> stateMap = new HashMap<>();
        for(String step : getSteps()) {
            stateMap.put(step, Collections.unmodifiableMap(getStepState(step).toMap()));
        }
        return Collections.unmodifiableMap(stateMap);
    }
    
    public void clearCachedStepStates() {
        stepStateMap.clear();
        request.removeAttribute(EFF_STEPSTATES_ATTR);
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
        m.put("website", WebsiteStepState.class);
        
        stepStateClsMap = Collections.unmodifiableMap(m);
    }
    
    private abstract class CommonStepState extends LazyResolvedStepState {
        public CommonStepState() { ; }
        public CommonStepState(ResolvedStepState partial) { super(partial); }

        @Override
        public boolean isSkippedImpl() {
            return Boolean.TRUE.equals(getSkippedSteps().get(getName()));
        }

        @Override
        public boolean isAccessibleImpl() {
            if (getInitialStep().equals(getName())) return true; // first step is always accessible
            else if (isCompleted()) return true; // SPECIAL CASE: if it's been filled-in, trust it can always be re-accessed for view or edit (out of order)
            else if (getStepIndex() <= getStepIndexWithFinished(determineStepAuto(true))) return true; // if the natural "next" step is us or a step after us, we should be accessible
            return false;
        }
        
        protected StepState getStepBefore() { return getStepState(SetupWorker.this.getStepBefore(getName())); }
        protected int getStepIndex() { return SetupWorker.this.getStepIndex(getName()); } // TODO: optimize static info
    }
    
    private class OrganizationStepState extends CommonStepState {
        public OrganizationStepState() { ; }
        public OrganizationStepState(ResolvedStepState partial) { super(partial); }
        
        @Override public String getName() { return "organization"; }
        @Override public boolean isSkippableImpl() { return true; }

        @Override
        public boolean isCompletedImpl() {
            try {
                List<GenericValue> partyRoles = getDelegator().findByAnd("PartyRole", UtilMisc.toMap("roleTypeId", "INTERNAL_ORGANIZATIO"), null, false);
                if (UtilValidate.isNotEmpty(partyRoles)) {
                    List<String> scpOrgPartyIdList = new ArrayList<>(partyRoles.size());
                    for(GenericValue partyRole : partyRoles) {
                        scpOrgPartyIdList.add(partyRole.getString("partyId"));
                    }
                    request.setAttribute("scpOrgPartyIdList", scpOrgPartyIdList);
                    request.setAttribute("scpOrgPartyRoleList", partyRoles);
                    return true;
                }
            } catch (GenericEntityException e) {
                throw new IllegalStateException("Setup: Database error: " + e.getMessage(), e);
            }
            return false;
        }
    }
    
    private class UserStepState extends CommonStepState {
        public UserStepState() { ; }
        public UserStepState(ResolvedStepState partial) { super(partial); }
        
        @Override public String getName() { return "user"; }
        @Override public boolean isSkippableImpl() { return true; }

        @Override
        public boolean isCompletedImpl() {
            return false;
        }
    }
    
    private class AccountingStepState extends CommonStepState {
        public AccountingStepState() { ; }
        public AccountingStepState(ResolvedStepState partial) { super(partial); }
        
        @Override public String getName() { return "accounting"; }
        @Override public boolean isSkippableImpl() { return true; }
        
        @Override
        public boolean isCompletedImpl() {
            return false;
        }
    }
    
    private class FacilityStepState extends CommonStepState {
        public FacilityStepState() { ; }
        public FacilityStepState(ResolvedStepState partial) { super(partial); }
        
        @Override public String getName() { return "facility"; }
        @Override public boolean isSkippableImpl() { return true; }

        @Override
        public boolean isCompletedImpl() {
            return false;
        }
    }
    
    private class CatalogStepState extends CommonStepState {
        public CatalogStepState() { ; }
        public CatalogStepState(ResolvedStepState partial) { super(partial); }
        
        @Override public String getName() { return "catalog"; }
        @Override public boolean isSkippableImpl() { return true; }

        @Override
        public boolean isCompletedImpl() {
            return false;
        }
    }
    
    private class WebsiteStepState extends CommonStepState {
        public WebsiteStepState() { ; }
        public WebsiteStepState(ResolvedStepState partial) { super(partial); }
        
        @Override public String getName() { return "website"; }
        @Override public boolean isSkippableImpl() { return true; }

        @Override
        public boolean isCompletedImpl() {
            return false;
        }
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
    
    private HttpSession getSession() {
        return request.getSession();
    }
}
