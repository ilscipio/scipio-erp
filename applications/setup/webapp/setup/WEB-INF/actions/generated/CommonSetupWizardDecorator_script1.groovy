import org.ofbiz.base.util.*;
                    context.setupStepAllowed = UtilMisc.booleanValueVersatile(parameters.setupForce, false) ||
                        UtilMisc.booleanValue(context.setupForce, false) ||
                        context.setupWorker?.isStepEffectiveAllowedSafe(context.setupStep);
                    // SPECIAL: record the "current" orgPartyId in session, ONLY so stock-like
                    // screens such as editcontactmech can access it (do not use if from scipio-code step screens!)
                    if (context.setupSessionOrgSet != false) {
                        session.setAttribute("scpSetupOrgPartyId", context.orgPartyId);
                    }