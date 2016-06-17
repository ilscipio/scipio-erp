package com.ilscipio.scipio.ce.demoSuite.dataGenerator.service;

public class DataGeneratorStat {

    private String entityName;
    private int stored = 0;
    private int failed = 0;

    DataGeneratorStat(String entityName) {
        this.entityName = entityName;
    }

    public String getEntityName() {
        return entityName;
    }

    // public void setEntityName(String entityName) {
    // this.entityName = entityName;
    // }
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
