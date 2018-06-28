import org.ofbiz.base.util.Debug;

import com.ilscipio.scipio.accounting.external.BaseOperationStats.NotificationLevel;
import com.ilscipio.scipio.accounting.external.BaseOperationStats.Stat;


final String module = "DatevImportResult.groovy";

if (parameters.operationStats) {
    List<Stat> fatalMessages = [];

    for (Stat stat : parameters.operationStats) {
        Debug.log("Scope [" + stat.getScope()+ "] + Level [" + stat.getLevel() + "]: " + stat.getMessage());
        if (stat.getLevel().equals(NotificationLevel.FATAL)) {
            fatalMessages.add(stat);
        }
    }
    
    context.stats = parameters.operationStats;
    context.fatalMessages = fatalMessages; 
}
