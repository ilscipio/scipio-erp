package com.ilscipio.scipio.ce.demoSuite.dataGenerator.service;

public class DataGeneratorStat {
    private String entityName;
    private int stored;
    private int failed;

    DataGeneratorStat(String entityName) {
        this.entityName = entityName;
    }

    public String getEntityName() {
        return entityName;
    }

    public int getStored() {
        return stored;
    }

    public void setStored(int stored) {
        this.stored = stored;
    }

    public int getFailed() {
        return failed;
    }

    public void setFailed(int failed) {
        this.failed = failed;
    }

}
