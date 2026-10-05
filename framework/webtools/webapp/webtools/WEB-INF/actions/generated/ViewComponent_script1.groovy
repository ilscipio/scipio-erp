compName = parameters.compName?.toString();
                    context.compName = compName;
                    compEnabled = org.ofbiz.base.component.ComponentConfig.isComponentEnabled(compName);
                    context.compEnabled = compEnabled;