package com.ilscipio.scipio.accounting.datev;

import java.util.List;

import com.ilscipio.scipio.accounting.datev.DatevNotificationMessage.NotificationMessageType;

public class DatevException extends Exception {
    private static final long serialVersionUID = -5965872197826715315L;

    private final DatevNotificationMessage datevNotificationMessage;

    public boolean isFatal() {
        if (datevNotificationMessage.getType().equals(NotificationMessageType.FATAL)) {
            return true;
        }
        return false;
    }

    public DatevException(DatevNotificationMessage notificationMessage) {
        super(notificationMessage.getNotificationMessage());
        this.datevNotificationMessage = notificationMessage;
    }

    public DatevException(DatevNotificationMessage notificationMessage, Throwable t) {
        super(t);
        this.datevNotificationMessage = notificationMessage;
    }

    public DatevException(DatevNotificationMessage notificationMessage, String message, Throwable t) {
        super(message, t);
        this.datevNotificationMessage = notificationMessage;
    }

    public DatevNotificationMessage getNotificationMessage() {
        return datevNotificationMessage;
    }

}
