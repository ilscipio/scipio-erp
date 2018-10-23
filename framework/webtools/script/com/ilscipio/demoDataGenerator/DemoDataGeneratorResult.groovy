import org.ofbiz.base.util.Debug

generatedDataStats = [];
if (session.getAttribute("_RUN_SYNC_RESULT_") && session.getAttribute("_RUN_SYNC_RESULT_").get("generatedDataStats")) {
    generatedDataStats = session.getAttribute("_RUN_SYNC_RESULT_").get("generatedDataStats");
    context.generatedDataStats = generatedDataStats;
    session.getAttribute("_RUN_SYNC_RESULT_").remove("generatedDataStats");
}