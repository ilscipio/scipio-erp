package com.ilscipio.scipio.accounting.external;

import java.util.LinkedList;
import java.util.List;
import java.util.Map;

public class BaseOperationStats {

    public BaseOperationStats() {
        super();
    }
    /**
     * Available modes to operate with
     * 
     * @author jsoto
     *
     */
    public enum NotificationScope {
        GLOBAL, META_HEADER, HEADER, RECORD
    }

    /**
     * Defines the type of the notification message:
     * 
     * FATAL: Ends the CSV parse process and the cause is stored for further
     * report.
     * 
     * ERROR: Stops the current parse sub process but continue with the pending
     * ones. Stores the casue for further report.
     * 
     * WARNING: Continues the CSV parse process but saves the notification
     * message for further report.
     * 
     * IGNORE: Continues the CSV parse and ignores whatever caused it.
     * 
     * @author jsoto
     *
     */
    public enum NotificationLevel {
        FATAL, ERROR, WARNING, IGNORE
    }

    List<Stat> stats = new LinkedList<Stat>();

    public void addStat(Stat stat) {
        stats.add(stat);
    }

    public List<Stat> getStats() {
        return stats;
    }

    public class Stat {
        final String message;

        final NotificationLevel level;
        final NotificationScope scope;
        // final OperationMode mode;

        public Stat(String message, NotificationScope scope, NotificationLevel level) {
            this.message = message;
            this.scope = scope;
            this.level = level;
        }

        public String getMessage() {
            return message;
        }

        public NotificationLevel getLevel() {
            return level;
        }

        public NotificationScope getScope() {
            return scope;
        }
    }

    public class RecordStat extends Stat {
        final int position;
        final Map<String, String> value;
        final boolean valid;

        public RecordStat(String message, NotificationLevel level, int position, Map<String, String> value, boolean valid) {
            super(message, NotificationScope.RECORD, level);
            this.value = value;
            this.valid = valid;
            this.position = position;
        }

        public String getMessage() {
            return message;
        }

        public Map<String, String> getValue() {
            return value;
        }

        public int getPosition() {
            return position;
        }

        public boolean isValid() {
            return valid;
        }
    }

}
