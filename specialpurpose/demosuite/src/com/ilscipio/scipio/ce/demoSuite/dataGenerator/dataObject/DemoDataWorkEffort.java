package com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject;

import java.sql.Timestamp;

public class DemoDataWorkEffort implements AbstractDataObject {

    private String id;
    private String name;
    private String type;
    private String status;
    private Timestamp estimatedStart;
    private Timestamp estimatedCompletion;
    private Timestamp actualStart;
    private Timestamp actualCompletion;

    private String partyStatus;
    private String assetStatus;

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public Timestamp getEstimatedStart() {
        return estimatedStart;
    }

    public void setEstimatedStart(Timestamp estimatedStart) {
        this.estimatedStart = estimatedStart;
    }

    public Timestamp getEstimatedCompletion() {
        return estimatedCompletion;
    }

    public void setEstimatedCompletion(Timestamp estimatedCompletion) {
        this.estimatedCompletion = estimatedCompletion;
    }

    public Timestamp getActualStart() {
        return actualStart;
    }

    public void setActualStart(Timestamp actualStart) {
        this.actualStart = actualStart;
    }

    public Timestamp getActualCompletion() {
        return actualCompletion;
    }

    public void setActualCompletion(Timestamp actualCompletion) {
        this.actualCompletion = actualCompletion;
    }

    public String getStatus() {
        return status;
    }

    public void setStatus(String status) {
        this.status = status;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public String getPartyStatus() {
        return partyStatus;
    }

    public void setPartyStatus(String partyStatus) {
        this.partyStatus = partyStatus;
    }

    public String getAssetStatus() {
        return assetStatus;
    }

    public void setAssetStatus(String assetStatus) {
        this.assetStatus = assetStatus;
    }

}
