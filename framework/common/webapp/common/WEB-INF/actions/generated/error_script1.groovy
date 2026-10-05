filterRequestUriError = request.getAttribute("filterRequestUriError");
                    if (filterRequestUriError) {
                        errMsg = org.ofbiz.base.util.UtilProperties.getMessage("CommonErrorUiLabels", "CommonServerRequestUrlNotFound",
                            [requestUrl:filterRequestUriError], context.locale);
                        errorMessageList = context.errorMessageList;
                        if (errorMessageList == null) {
                            errorMessageList = [];
                            context.errorMessageList = errorMessageList;
                        }
                        errorMessageList.add(errMsg);
                    }