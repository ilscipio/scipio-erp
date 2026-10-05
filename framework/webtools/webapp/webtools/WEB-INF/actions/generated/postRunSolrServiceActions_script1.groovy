import org.ofbiz.base.util.*;
                import org.ofbiz.service.*;
                
                if (parameters._SOLR_SRV_RUN_ == "Y") {
                    serviceResult = session.getAttribute("_RUN_SYNC_RESULT_");
                    if (serviceResult) {
                        responseStatus = serviceResult[ModelService.RESPONSE_MESSAGE];
                        if (ServiceUtil.isSuccess(serviceResult)) {
                            // simply squash the 'scheduled' success message, useless
                            successMsg = ServiceUtil.getSuccessMessage(serviceResult);
                            if (successMsg) {
                                context.eventMessageList = [successMsg];
                            }
                        } else {
                            // get rid of the event message, but make sure
                            // to preserve the error message list, just append to it
                            context.eventMessageList = [];
                            errorMessageList = context.errorMessageList;
                            if (errorMessageList == null) errorMessageList = [];
                            errorMsg = ServiceUtil.getErrorMessage(serviceResult);
                            errorMessageList.add(errorMsg + " (responseMessage: " + responseStatus + ")");
                            context.errorMessageList = errorMessageList;
                        }
                    }
                }