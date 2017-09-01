package com.ilscipio.scipio.setup;

import java.io.Serializable;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;

/**
 * SCIPIO: Setup worker.
 */
@SuppressWarnings("serial")
public class SetupWorker implements Serializable {

    public static final String module = SetupWorker.class.getName();
    
    public static final String FINISHED_STEP = "finished";
    public static final String ERROR_STEP = "error";
    
    /**
     * "Next" setup step that can be requested via req params or attributes (is verified for valid).
     */
    public static final String REQ_SETUP_STEP_ATTR = "scpSetupStep";
    
    /**
     * The step we are submitting form for ("last" step).
     */
    public static final String SUBMIT_SETUP_STEP_ATTR = "scpSubmitSetupStep";
    
    /**
     * Effective setup step, cached in request after event or first access.
     */
    public static final String EFF_SETUP_STEP_ATTR = "scpEffSetupStep";

    private static final List<String> setupSteps = UtilMisc.unmodifiableArrayList(
            "organization", "user", "accounting", "facility", "productstore", "website"
    );
    private static final Set<String> setupStepsSet = Collections.unmodifiableSet(new HashSet<String>(setupSteps));
    private static final Set<String> allSetupStepValuesSet;
    static {
        Set<String> s = new HashSet<>(setupStepsSet);
        s.add(FINISHED_STEP);
        s.add(ERROR_STEP);
        allSetupStepValuesSet = Collections.unmodifiableSet(s);
    }
    
    private static final String initialStep = setupSteps.get(0);
    
    private static final Map<String, Boolean> skipAllowMap;
    static {
        Map<String, Boolean> m = new HashMap<>();
        m.put("organization", true);
        m.put("user", true);
        m.put("accounting", true);
        m.put("facility", true);
        m.put("productstore", true);
        m.put("website", true);
        skipAllowMap = Collections.unmodifiableMap(m);
    }
    
    private final HttpServletRequest request;
    private transient Delegator delegator;
    
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
    
    public List<String> getSetupSteps() { return setupSteps; }
    public Set<String> getSetupStepsSet() { return setupStepsSet; }
    public Set<String> getAllSetupStepValuesSet() { return allSetupStepValuesSet; }
    public String getInitialStep() { return initialStep; }
    public int getSetupStepIndex(String step) { return setupSteps.indexOf(step); }
    
    /* 
     * *******************************************
     * Stateful queries
     * *******************************************
     */
    
    /**
     * Returns last submitted setup step (rather than "current"/"next").
     */
    public String getSubmittedSetupStep() {
        String lastStep = (String) request.getAttribute(SUBMIT_SETUP_STEP_ATTR);
        if (lastStep == null) {
            lastStep = request.getParameter(SUBMIT_SETUP_STEP_ATTR);
        }
        return lastStep;
    }
    
    /**
     * WARN: must be verified.
     */
    public String getRequestedSetupStep() {
        String step = (String) request.getAttribute(REQ_SETUP_STEP_ATTR);
        if (step == null) {
            step = request.getParameter(REQ_SETUP_STEP_ATTR);
        }
        return step;
    }
    
    public String getEffectiveSetupStep() {
        return (String) request.getAttribute(EFF_SETUP_STEP_ATTR);
    }
    
    public void setEffectiveSetupStep(String effectiveStep) {
        request.setAttribute(EFF_SETUP_STEP_ATTR, effectiveStep);
    }
    
    /**
     * Returns the "current"/"next" step.
     * In all cases, if the EFF_SETUP_STEP_ATTR is set, everything else is ignored.
     * <p>
     * NOTE: Upon review, screens don't need to call this; they set a context var and just
     * need to validate it with {@link #isSetupStepEffectiveSafe}.
     * <p>
     * @param useRequested if TRUE check requested and error if invalid; if FALSE ignore requested; 
     *                     if null check requested but fallback if invalid
     * @param updateEffective if TRUE always update effective even if exception (sets "error"); if FALSE never update;
     *                        if null only update if non-exception
     * @see #getSubmittedSetupStep()
     */
    public String getSetupStep(Boolean useRequested, Boolean updateEffective) throws Exception {
        String step = getEffectiveSetupStep();
        if (step == null) {
            try {
                step = determineSetupStep(useRequested);
                if (!Boolean.FALSE.equals(updateEffective)) setEffectiveSetupStep(step);
            } catch(Exception e) {
                if (Boolean.TRUE.equals(updateEffective)) setEffectiveSetupStep(ERROR_STEP);
                throw e;
            }
        }
        return step;
    }
    
    // TODO: REVIEW: might want to split this up to handle bad request separately
    public String determineSetupStep(Boolean useRequested) throws Exception {
        String requestedStep = !Boolean.FALSE.equals(useRequested) ? getRequestedSetupStep() : null;
        if (UtilValidate.isNotEmpty(requestedStep)) {
            if (Boolean.TRUE.equals(useRequested)) { 
                if (isSetupStepAllowed(requestedStep)) return requestedStep;
                else throw new IllegalArgumentException("setup step requested cannot be fulfilled (prior steps required): " + requestedStep);
            } else {
                try {
                    if (isSetupStepAllowed(requestedStep)) return requestedStep;
                } catch(Exception e) {
                    ;
                }
            }
        } 
        
        for(String step : getSetupSteps()) {
            if (!isSetupStepSatisfied(step) && !isSetupStepSkipped(step)) {
                return step;
            }
        }
        return FINISHED_STEP;
    }
    
    public boolean isSetupStepSatisfied(String step) throws Exception {
        
        // TODO
        getDelegator(); //...
        
        return false;
    }
    
    public boolean isSetupStepSkippable(String step) {
        return Boolean.TRUE.equals(skipAllowMap.get(step));
    }
    
    public boolean isSetupStepSkipped(String step) {
        if (!isSetupStepSkippable(step)) return false;
        
        // TODO
        getSession(); //...
        
        return false;
    }
    
    public boolean isSetupStepAllowed(String requestedStep) throws Exception {
        if (ERROR_STEP.equals(requestedStep)) return true;
        if (!getAllSetupStepValuesSet().contains(requestedStep)) {
            throw new IllegalArgumentException("invalid setup step requested: " + requestedStep);
        }
        
        for(String step : getSetupSteps()) {
            if (step.equals(requestedStep)) {
                return true;
            }
            
            if (!isSetupStepSkippable(step) && !isSetupStepSatisfied(step)) {
                return false;
            }
        }
        
        return FINISHED_STEP.equals(requestedStep);
    }
    
    /**
     * If the effective is set, checks if the step matches it; if effective is not set, checks if the step is allowed.
     * FOR USE IN SCREENS to prevent bypassing the wizard.
     */
    public boolean isSetupStepEffectiveSafe(String step) {
        if (UtilValidate.isEmpty(step)) return true;
        try {
            String effStep = getEffectiveSetupStep();
            if (effStep != null) { 
                // if an eff step was set, we have to match it exactly
                return effStep.equals(step);
            } else { 
                // if there was no eff step (something was bypassed), we could just return false, 
                // but can just check it this way instead... don't think can hurt
                return isSetupStepAllowed(step);
            }
        } catch (Exception e) {
            Debug.logError("Setup: Error determining setup step: " + e.getMessage(), module);
            return false;
        }
    }

    
    /* 
     * *******************************************
     * Local Helpers
     * *******************************************
     */
    
    protected Delegator getDelegator() {
        Delegator delegator = this.delegator;
        if (delegator == null) {
            delegator = (Delegator) request.getAttribute("delegator");
            this.delegator = delegator;
        }
        return delegator;
    }
    
    protected HttpSession getSession() {
        return request.getSession();
    }
}
