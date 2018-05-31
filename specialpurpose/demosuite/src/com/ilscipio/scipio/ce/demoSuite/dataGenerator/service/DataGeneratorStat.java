package com.ilscipio.scipio.ce.demoSuite.dataGenerator.service;

import java.util.ArrayList;
import java.util.List;

import org.ofbiz.entity.GenericValue;

public class DataGeneratorStat {
    private String entityName;
    private int stored;
    private int failed;
    private List<GenericValue> generatedValues;

    DataGeneratorStat(String entityName) {
        this.entityName = entityName;
        this.setGeneratedValues(new ArrayList<>());
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

    public List<GenericValue> getGeneratedValues() {
        return generatedValues;
    }

    public void setGeneratedValues(List<GenericValue> generatedValues) {
        this.generatedValues = generatedValues;
    }

}
