package com.ilscipio.scipio.solr;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.service.DispatchContext;

/**
 * Collects status during SolrProductSearch.updateToSolrCoreMultiAdd and rebuildSolrIndex.
 * Available to {@link IndexingHookHandler}.
 */
public interface IndexingStatus {

    DispatchContext getDctx();
    IndexingHookHandler.HookType getHookType();
    SolrProductIndexer getIndexer();
    int getBufSize();
    int getMaxDocs();
    int getNumDocsToIndex();
    int getNumFailures();
    int getStartIndex();
    int getEndIndex();
    int getHookFailures();
    default String getIndexProgressString() { return getStartIndex() + "-" + getEndIndex() + " / " + getMaxDocs(); }

    class Standard implements IndexingStatus {
        private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

        private final DispatchContext dctx;
        private final IndexingHookHandler.HookType hookType;
        private final SolrProductIndexer indexer;
        private final String logPrefix;
        private int bufSize;
        private int maxDocs;
        private int numDocsToIndex;
        private int numFailures;
        private int startIndex;
        private int endIndex;
        private int hookFailures;

        public Standard(DispatchContext dctx, IndexingHookHandler.HookType hookType, SolrProductIndexer indexer, int maxDocs, String logPrefix) {
            this.dctx = dctx;
            this.hookType = hookType;
            this.indexer = indexer;
            this.logPrefix = logPrefix != null ? logPrefix : "";
            this.bufSize = UtilProperties.getPropertyAsInteger(SolrUtil.solrConfigName, "solr.index.rebuild.record.buffer.size", 1000);
            this.maxDocs = maxDocs;
            this.numDocsToIndex = 0;
            this.numFailures = 0;
            this.startIndex = 1;
            this.endIndex = -1; // calculated on first loop
            this.hookFailures = 0;
        }

        public DispatchContext getDctx() { return dctx; }
        public IndexingHookHandler.HookType getHookType() {
            return hookType;
        }
        public SolrProductIndexer getIndexer() {
            return indexer;
        }
        public String getLogPrefix() { return logPrefix; }
        public int getBufSize() { return bufSize; }
        public int getMaxDocs() {
            return maxDocs;
        }
        public int getNumDocsToIndex() {
            return numDocsToIndex;
        }
        public int getNumFailures() {
            return numFailures;
        }
        public int getStartIndex() {
            return startIndex;
        }
        public int getEndIndex() {
            return endIndex;
        }
        public int getHookFailures() {
        return hookFailures;
    }

        public void setBufSize(int bufSize) { this.bufSize = bufSize; }
        public void setMaxDocs(int maxDocs) { this.maxDocs = maxDocs; }
        public void setNumDocsToIndex(int numDocsToIndex) { this.numDocsToIndex = numDocsToIndex; }
        public void increaseNumDocsToIndex(int amount) { this.numDocsToIndex += amount; }
        public void setNumFailures(int numFailures) { this.numFailures = numFailures; }
        public void registerGeneralFailure(String msg, Throwable t) {
            this.numFailures++;
            Debug.logError(t, getLogPrefix() + msg + getErrorLogSuffix(), module);
        }
        public void setStartIndex(int startIndex) { this.startIndex = startIndex; }
        public void increaseStartIndex(int amount) { this.startIndex += amount; }
        public void setEndIndex(int endIndex) { this.endIndex = endIndex; }
        public void setHookFailures(int hookFailures) { this.hookFailures = hookFailures; }
        public void registerHookFailure(String msg, Throwable t, IndexingHookHandler hookHandler, String methodName) {
            this.hookFailures++;
            Debug.logError(t, getLogPrefix()+ "Error in hook handler " + hookHandler + " method [" + methodName + "]" +
                    (msg != null ? ": " + msg : "") + getErrorLogSuffix(), module);
        }
        public void updateStartEndIndex(int docsConsumed) {
            increaseStartIndex(docsConsumed);
            // NOTE: the endIndex is actually a prediction, but if it's ever false, there is a serious DB problem
            if (getBufSize() > 0) {
                setEndIndex(getStartIndex() + Math.min(getBufSize(), getMaxDocs() - (getStartIndex() - 1)) - 1);
            } else {
                setEndIndex(getMaxDocs());
            }
        }
        public String getErrorLogSuffix() { return "; total general errors: " + getNumFailures() + "; total hook errors: " + getHookFailures(); }
    }
}
