if (!context.serviceNames) {
                        context.serviceNames = dispatcher.getDispatchContext().getAllServiceNames();
                    }
                    context.serviceNamesMatchMode = context.serviceNamesMatchMode ?: "best";
                    context.serviceNamesMaxMatch = context.serviceNamesMaxMatch ?: -1;
                    if (!context.serviceNameInputIdExpr) {
                        context.serviceNameInputIdExpr = "input[name=SERVICE_NAME]";
                    }