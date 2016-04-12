package com.ilscipio.cato.util;

import java.io.PrintStream;

import org.apache.tools.ant.BuildEvent;
import org.apache.tools.ant.DefaultLogger;

public class CatoLogger extends DefaultLogger {

    @Override
    public void setMessageOutputLevel(int level) {
        // TODO Auto-generated method stub
        super.setMessageOutputLevel(level);
    }

    @Override
    public void setOutputPrintStream(PrintStream output) {
        // TODO Auto-generated method stub
        super.setOutputPrintStream(System.out);
    }

    @Override
    public void setErrorPrintStream(PrintStream err) {
        // TODO Auto-generated method stub
        super.setErrorPrintStream(err);
    }

    @Override
    public void setEmacsMode(boolean emacsMode) {
        // TODO Auto-generated method stub
        super.setEmacsMode(emacsMode);
    }

    @Override
    public void buildStarted(BuildEvent event) {
        // TODO Auto-generated method stub
//        super.buildStarted(event);
    }

    @Override
    public void buildFinished(BuildEvent event) {
        // TODO Auto-generated method stub
//        super.buildFinished(event);
    }

    @Override
    protected String getBuildFailedMessage() {
        // TODO Auto-generated method stub
        return super.getBuildFailedMessage();
    }

    @Override
    protected String getBuildSuccessfulMessage() {
        // TODO Auto-generated method stub
        
//        return super.getBuildSuccessfulMessage();
        return "";
    }

    @Override
    public void targetStarted(BuildEvent event) {
        // TODO Auto-generated method stub
//        super.targetStarted(event);
    }

    @Override
    public void targetFinished(BuildEvent event) {
        // TODO Auto-generated method stub
//        super.targetFinished(event);
    }

    @Override
    public void taskStarted(BuildEvent event) {
        // TODO Auto-generated method stub
//        super.taskStarted(event);
    }

    @Override
    public void taskFinished(BuildEvent event) {
        // TODO Auto-generated method stub
//        super.taskFinished(event);
    }

    @Override
    public void messageLogged(BuildEvent event) {
        // TODO Auto-generated method stub
        super.messageLogged(event);
    }

    @Override
    protected void printMessage(String message, PrintStream stream, int priority) {
        // TODO Auto-generated method stub 
        
        super.printMessage(message, stream, priority);
    }

    @Override
    protected void log(String message) {
        // TODO Auto-generated method stub
        super.log(message);
    }

    @Override
    protected String getTimestamp() {
        // TODO Auto-generated method stub
        return super.getTimestamp();
    }

    @Override
    protected String extractProjectName(BuildEvent event) {
        // TODO Auto-generated method stub
        return super.extractProjectName(event);
    }

    @Override
    public int hashCode() {
        // TODO Auto-generated method stub
        return super.hashCode();
    }

    @Override
    public boolean equals(Object obj) {
        // TODO Auto-generated method stub
        return super.equals(obj);
    }

    @Override
    protected Object clone() throws CloneNotSupportedException {
        // TODO Auto-generated method stub
        return super.clone();
    }

    @Override
    public String toString() {
        // TODO Auto-generated method stub
        return super.toString();
    }

    @Override
    protected void finalize() throws Throwable {
        // TODO Auto-generated method stub
        super.finalize();
    }
        

}
