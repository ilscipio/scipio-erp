errorMessageList = context.errorMessageList;
                        if (errorMessageList == null) {
                            errorMessageList = [];
                            context.errorMessageList = errorMessageList;
                        }
                        errorMessageList.add(org.ofbiz.base.util.UtilProperties.getMessage('AccountingUiLabels', 
                            'AccountingGlAccountNotFound', context, context.locale));