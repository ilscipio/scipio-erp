context.testIt = delegator.query().from("WebSite").queryIterator();
                    class CatchActionsTestException extends RuntimeException {}
                    context.testException = new CatchActionsTestException();
                    Debug.logInfo("CatchActionsTest: throwing exception: " + context.testException, "CatchActionsTest.groovy");
                    // NOTE: this can be used, but there is extra ugly warning from ScriptUtil
                    //throw context.testException;