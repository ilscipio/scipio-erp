package org.ofbiz.base.util;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * SCIPIO: Helper map processor invoker.
 */
public class MapProcessorInvoker {

    protected Map<String, Object> inMap;

    protected Map<String, List<String>> entryErrorMessages;
    protected List<String> generalErrorMessages;
    protected Map<String, Object> lastResults = null;
    protected Map<String, Object> combinedResults = new HashMap<String, Object>();

    protected Locale locale;

    public MapProcessorInvoker(Map<String, Object> inMap,
            Map<String, List<String>> entryErrorMessages, List<String> errorMessages, Locale locale) {
        super();
        this.inMap = inMap;
        this.entryErrorMessages = entryErrorMessages;
        this.generalErrorMessages = errorMessages;
        this.locale = locale;
    }

    public MapProcessorInvoker(Map<String, Object> inMap, Locale locale) {
        super();
        this.inMap = inMap;
        this.entryErrorMessages = new HashMap<String, List<String>>();
        this.generalErrorMessages = new ArrayList<String>();
        this.locale = locale;
    }

    public static MapProcessorInvoker getInstance(Map<String, Object> inMap, Locale locale) {
        return new MapProcessorInvoker(inMap, locale);
    }

    public void process(Collection<MapProcessor> processors, boolean resultsToInMap, boolean resultsToCombined) throws GeneralException {
        Map<String, Object> results = new HashMap<String, Object>();
        for(MapProcessor processor : processors) {
            processor.process(inMap, results, entryErrorMessages, generalErrorMessages, locale);
            lastResults = results;
            if (resultsToInMap) {
                inMap.putAll(results);
            }
            if (resultsToCombined) {
                combinedResults.putAll(results);
            }
        }
    }

    public void process(MapProcessor processor, boolean resultsToInMap, boolean resultsToCombined) throws GeneralException {
        process(Arrays.asList(new MapProcessor[] { processor }), resultsToInMap, resultsToCombined);
    }

    public void process(boolean resultsToInMap, boolean resultsToCombined, MapProcessor... processors) throws GeneralException {
        process(Arrays.asList(processors), resultsToInMap, resultsToCombined);
    }

    public void process(MapProcessor[] processors, boolean resultsToInMap, boolean resultsToCombined) throws GeneralException {
        process(Arrays.asList(processors), resultsToInMap, resultsToCombined);
    }

    public Map<String, Object> getLastResults() {
        return lastResults;
    }

    public Map<String, Object> getCombinedResults() {
        return combinedResults;
    }

    public Map<String, Object> getFinalMap() {
        return inMap;
    }

    public Map<String, List<String>> getEntryErrorMessages() {
        return entryErrorMessages;
    }

    public List<String> getGeneralErrorMessages() {
        return generalErrorMessages;
    }

    public List<String> getAllErrorMessages() {
        return getAllErrorMessages(entryErrorMessages, generalErrorMessages);
    }

    public Locale getLocale() {
        return locale;
    }

    public static void addEntryErrorMessagesToList(Map<String, List<String>> entryErrorMessages, List<String> outErrorMessages) {
        for(List<String> list : entryErrorMessages.values()) {
            outErrorMessages.addAll(list);
        }
    }

    public static List<String> getAllErrorMessages(Map<String, List<String>> entryErrorMessages, List<String> errorMessages) {
        List<String> res = new ArrayList<String>();
        res.addAll(errorMessages);
        addEntryErrorMessagesToList(entryErrorMessages, res);
        return res;
    }


}
