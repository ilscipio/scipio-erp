import org.ofbiz.base.util.*;
                    final module = "SetupOrganizationMain.groovy";
                    errorMsg = null;
                    try {
                        request.setAttribute("scpSetEffSetupStep", "organization");
                        result = com.ilscipio.scipio.setup.SetupEvents.setEffectiveSetupStep(request, response);
                        if ("error".equals(result)) {
                            errorMsg = request.getAttribute("_ERROR_MESSAGE_");
                            request.removeAttribute("_ERROR_MESSAGE_");
                        }
                    } catch(Exception e) {
                        Debug.logError(e, "Error setting effective setup step", module);
                        errorMsg = e.toString();
                    }
                    if (errorMsg) {
                        errorMsg = UtilProperties.getMessage("ScipioSetupErrorUiLabels", "SetupError",
                            context.locale) + ": " + errorMsg;
                        errorMessageList = context.errorMessageList;
                        if (errorMessageList == null) {
                            errorMessageList = [];
                            context.errorMessageList = errorMessageList;
                        }
                        errorMessageList.add(errorMsg)
                    }
                    context.setStepSuccess = !errorMsg;