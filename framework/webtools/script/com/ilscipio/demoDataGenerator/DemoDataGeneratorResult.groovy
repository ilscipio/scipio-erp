import org.ofbiz.base.util.Debug

if (session.getAttribute("_RUN_SYNC_RESULT_") && session.getAttribute("_RUN_SYNC_RESULT_").get("generatedData")) {
    context.generatedData = session.getAttribute("_RUN_SYNC_RESULT_").get("generatedData");
    Debug.log("generatedData exists");
}