import javax.transaction.Transaction;
                    import org.ofbiz.base.util.*;
                    import org.ofbiz.entity.transaction.*;
                    final module = "TemplateTest.groovy";

                    scriptBody = parameters.scriptBody as String;
                    templateBody = parameters.templateBody as String;

                    execDefault = (scriptBody == null && templateBody == null);
                    if (scriptBody == null) {
                        scriptBody = "import org.ofbiz.base.util.*;\n" +
                            "import org.ofbiz.entity.condition.*;\n" +
                            "import org.ofbiz.entity.util.*;\n" +
                            "\n" +
                            "final module = \"TemplateTest.groovy\";\n" +
                            "\n" +
                            "userParty = from(\"Party\").select(\"partyId\", \"partyTypeId\").where(\"partyId\", context.userLogin?.partyId).queryOne();\n" +
                            "\n" +
                            "Debug.logInfo(\"Hello from test script and user party: \" + userParty, module);\n" +
                            "\n" +
                            "context.testVar1 = \"This is a test string value from groovy from user party: \" + userParty;\n";
                    }
                    if (templateBody == null) {
                        templateBody = "<#assign ftlTestVar1 = testVar1!>" +
                            "\n" +
                            "\n<p><strong>testVar1:</strong> <em>\${escapeVal(ftlTestVar1, 'htmlmarkup')}</em>";
                    }

                    execTemplate = (execDefault || request.getMethod().toLowerCase() == "post") && context.hasTmplTestPerm == true; // security
                    if (execTemplate && scriptBody) {
                        Transaction suspendedTransaction = null;
                        try {
                            if (TransactionUtil.isTransactionInPlace()) { // SCIPIO: 2018-09-04: added check to eliminate useless warnings
                                suspendedTransaction = TransactionUtil.suspend();
                            }
                            beganTransaction = false;
                            try {
                                beganTransaction = TransactionUtil.begin(72000);
                                GroovyUtil.evalBlock(scriptBody, context, false);
                                TransactionUtil.commit(beganTransaction);
                            } catch (Exception e) {
                                Debug.logError(e, "Error evaluating Groovy script", module);
                                errorMessageList = context.errorMessageList ?: [];
                                errorMessageList.add("Error evaluating Groovy script: " + e.toString());
                                context.errorMessageList = errorMessageList;
                                try {
                                    TransactionUtil.rollback(beganTransaction, "Error evaluating Groovy script", e);
                                } catch (GenericTransactionException e2) {
                                    Debug.logError(e2, "Unable to rollback transaction", module);
                                }
                            }
                        } finally {
                            if (suspendedTransaction != null) {
                                try {
                                    TransactionUtil.resume(suspendedTransaction);
                                } catch (GenericTransactionException e) {
                                    Debug.logError(e, "Error resuming suspended transaction", module);
                                }
                            }
                        }
                    }

                    context.scriptBody = scriptBody;
                    context.templateBody = templateBody;
                    context.execDefault = execDefault;
                    context.execTemplate = execTemplate;