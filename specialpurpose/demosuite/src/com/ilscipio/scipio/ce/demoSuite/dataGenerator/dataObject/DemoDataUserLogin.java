package com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject;

public class DemoDataUserLogin implements AbstractDataObject {

    private String userLoginId;
    private String currentPassword;

    public String getUserLoginId() {
        return userLoginId;
    }

    public void setUserLoginId(String userLoginId) {
        this.userLoginId = userLoginId;
    }

    public String getCurrentPassword() {
        return currentPassword;
    }

    public void setCurrentPassword(String currentPassword) {
        this.currentPassword = currentPassword;
    }

}
