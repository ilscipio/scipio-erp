import org.ofbiz.base.util.Debug

if (session.getAttribute("_RUN_SYNC_RESULT_") && session.getAttribute("_RUN_SYNC_RESULT_").get("generatedDataStats")) {
    context.generatedDataStats = session.getAttribute("_RUN_SYNC_RESULT_").get("generatedDataStats");
//    Debug.log("generatedData exists");
}

//for (c in context) {
//    Debug.log("c ====> " + c);
//}