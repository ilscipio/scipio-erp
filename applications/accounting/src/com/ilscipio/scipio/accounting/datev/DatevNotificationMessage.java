package com.ilscipio.scipio.accounting.datev;

public class DatevNotificationMessage {
    final NotificationMessageType type;
    final String notificationMessage;

    /**
     * Defines the type of the notification message: FATAL: Stops the CSV parse
     * process and saves the cause gets collected for further report. WARNING:
     * Continues the CSV parse process but saves the notification message for
     * further report. IGNORE: Continues the CSV parse and ignores whatever
     * caused it.
     * 
     * @author jsoto
     *
     */
    public enum NotificationMessageType {
        FATAL, RECORD_ERROR, WARNING, IGNORE
    }

    public DatevNotificationMessage(NotificationMessageType notificationMessageType, String message) {
        this.type = notificationMessageType;
        this.notificationMessage = message;
    }

    public NotificationMessageType getType() {
        return type;
    }

    public String getNotificationMessage() {
        return notificationMessage;
    }
}
