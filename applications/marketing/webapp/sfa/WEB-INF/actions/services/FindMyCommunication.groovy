context.communicationEventTypeList = from("CommunicationEventType").queryList();

context.comEventStatusList = from("StatusItem").where(["statusTypeId" : "COM_EVENT_STATUS"]).queryList();
context.comEventRoleStatusList = from("StatusItem").where(["statusTypeId" : "COM_EVENT_ROL_STATUS"]).queryList();