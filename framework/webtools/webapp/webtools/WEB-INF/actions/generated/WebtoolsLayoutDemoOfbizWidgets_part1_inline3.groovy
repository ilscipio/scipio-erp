                                    public class TestUtil {
                                        def logVars(context, label) {
                                            org.ofbiz.base.util.Debug.logInfo(label + ": mySimpleVar1: " + context.mySimpleVar1
                                                + "; mySimpleVar2: " + context.mySimpleVar2, "IncludeActionsTestGroovy");
                                        }
                                    }
                                    context.testUtil = new TestUtil();
                                    context.remove("mySimpleVar2");
                                
