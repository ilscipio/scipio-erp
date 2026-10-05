import org.ofbiz.base.util.*;
                        
                        Debug.logInfo("\n\n"
                            + "******************************************************\n"
                            + "Running layout demo in Debug mode\n"
                            + "******************************************************\n"
                            + "Note: This screen intentionally causes some log warnings and errors for testing purposes.\n"
                            + "See component://webtools/widget/MiscScreens.xml#WebtoolsLayoutDemo and related resources for details.\n"
                            + "URL: " + request.getRequestURL() + (request.getQueryString() ? "?" + request.getQueryString() : "") + "\n"
                            , "LayoutDemoActions.groovy");
                        
                        Debug.logInfo("Testing GeneralException messages...", "LayoutDemoActions.groovy");
                        try {
                            throw new GeneralException();
                        } catch(GeneralException e) {
                            Debug.logInfo("GeneralException getMessage(): " + e.getMessage(), "LayoutDemoActions.groovy");
                        }
                        try {
                            throw new GeneralException("Main exception detail message");
                        } catch(GeneralException e) {
                            Debug.logInfo("GeneralException getMessage(): " + e.getMessage(), "LayoutDemoActions.groovy");
                        }
                        try {
                            throw new GeneralException("Main exception detail message").setPropertyMessage(PropertyMessage.makeFromStatic("Main exception detail property message"));
                        } catch(GeneralException e) {
                            Debug.logInfo("GeneralException getMessage(): " + e.getMessage(), "LayoutDemoActions.groovy");
                        }
                        try {
                            throw new GeneralException(new IllegalArgumentException("a nested exception")).setPropertyMessage(PropertyMessage.makeFromStatic("Main exception detail property message"));
                        } catch(GeneralException e) {
                            Debug.logInfo("GeneralException getMessage(): " + e.getMessage(), "LayoutDemoActions.groovy");
                        }
                        try {
                            msgs = [];
                            msgs.add(PropertyMessage.make("WebtoolsUiLabels", "WebtoolsTestLabel"));
                            msgs.add("Static string message 1, non-localized");
                            msgs.add(PropertyMessage.make("WebtoolsUiLabels", "WebtoolsTestLabel_en"));
                            msgs.add("Static string message 2, non-localized");
                            msgs.add(PropertyMessage.make("WebtoolsUiLabels", "WebtoolsTestLabel_de"));
                            throw new GeneralException("Main exception detail message", msgs).setPropertyMessage(PropertyMessage.makeFromStatic("Main exception detail property message"));
                        } catch(GeneralException e) {
                            Debug.logInfo("GeneralException getMessage(): " + e.getMessage(), "LayoutDemoActions.groovy");
                            Debug.logInfo("GeneralException messages (default prop locale): " + e.getMessageList().toString(), "LayoutDemoActions.groovy");
                            Debug.logInfo("GeneralException messages (default log locale): " + PropertyMessage.getLogMessages(e.getPropertyMessageList()).toString(), "LayoutDemoActions.groovy");
                            Debug.logInfo("GeneralException messages (german): " + e.getMessageList(Locale.GERMAN).toString(), "LayoutDemoActions.groovy");
                            Debug.logInfo("GeneralException messages (german 2): " + PropertyMessage.getMessages(e.getPropertyMessageList(), Locale.GERMAN).toString(), "LayoutDemoActions.groovy");
                        }
                        try {
                            msgs = [];
                            msgs.add(PropertyMessage.make("WebtoolsUiLabels", "WebtoolsTestLabel"));
                            msgs.add("Static string message 1, non-localized");
                            msgs.add(PropertyMessage.make("WebtoolsUiLabels", "WebtoolsTestLabel_en"));
                            msgs.add("Static string message 2, non-localized");
                            msgs.add(PropertyMessage.make("WebtoolsUiLabels", "WebtoolsTestLabel_de"));
                            throw new GeneralException("Main exception detail message", msgs, new IllegalArgumentException("a nested exception"));
                        } catch(GeneralException e) {
                            Debug.logInfo("GeneralException getMessage(): " + e.getMessage(), "LayoutDemoActions.groovy");
                            Debug.logInfo("GeneralException messages (default prop locale): " + e.getMessageList().toString(), "LayoutDemoActions.groovy");
                            Debug.logInfo("GeneralException messages (default log locale): " + PropertyMessage.getLogMessages(e.getPropertyMessageList()).toString(), "LayoutDemoActions.groovy");
                            Debug.logInfo("GeneralException messages (german): " + e.getMessageList(Locale.GERMAN).toString(), "LayoutDemoActions.groovy");
                            Debug.logInfo("GeneralException messages (german 2): " + PropertyMessage.getMessages(e.getPropertyMessageList(), Locale.GERMAN).toString(), "LayoutDemoActions.groovy");
                        }