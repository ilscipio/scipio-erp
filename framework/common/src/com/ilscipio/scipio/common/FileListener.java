/*
 * Scipio Commerce
 * Copyright (C) Ilscipio GmbH
 *
 * This file is part of Scipio Commerce. Scipio Commerce is free software: you
 * can redistribute it and modify it under the terms of the GNU Affero General
 * Public License, version 3, as published by the Free Software Foundation.
 * Scipio Commerce is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
 * for more details. You should have received a copy of the license with this
 * work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
 * A commercial license is available from Ilscipio GmbH.
 *
 * SPDX-License-Identifier: AGPL-3.0-only
 */
package com.ilscipio.scipio.common;

import java.io.IOException;
import java.net.URL;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.StandardWatchEventKinds;
import java.nio.file.WatchEvent;
import java.nio.file.WatchKey;
import java.nio.file.WatchService;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.concurrent.ScheduledExecutorService;

import org.apache.commons.io.FilenameUtils;
import org.ofbiz.base.concurrent.ExecutionPool;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilURL;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.DelegatorFactory;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceContainer;

/**
 * Scipio File Listener
 *
 * Implementes WatchService for specific directories and triggers service based on events
 *
 */
public class FileListener {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    private static final ThreadGroup FILE_LISTENER_THREAD_GROUP = new ThreadGroup("WatchServices");
    private static final Delegator delegator = DelegatorFactory.getDelegator("default");
    private static final LocalDispatcher dispatcher = ServiceContainer.getLocalDispatcher(delegator.getDelegatorName(), delegator);
    /**
     *  Start a file listener (WatchService) and run a service of a given name and writes the result to the database
     *  Can be used to implements EECAs to auto-update information based on file changes
     **/
    public static void startFileListener(String name, String location){

        try{
            if(UtilValidate.isNotEmpty(name) && UtilValidate.isNotEmpty(location)){

                if(getThreadByName(name)!=null){
                    Debug.logInfo("Filelistener "+name+" already started. Skipping...", module);
                }else{
                    URL resLocation = UtilURL.fromResource(location);
                    Path folderLocation = Paths.get(resLocation.toURI());
                    if(folderLocation == null) {
                        throw new UnsupportedOperationException("Directory not found");
                    }


                    final WatchService folderWatcher = folderLocation.getFileSystem().newWatchService();
                    // register all subfolders
                    Files.walkFileTree(folderLocation, new SimpleFileVisitor<Path>() {
                        @Override
                        public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs) throws IOException {
                            dir.register(folderWatcher, StandardWatchEventKinds.ENTRY_CREATE, StandardWatchEventKinds.ENTRY_DELETE,StandardWatchEventKinds.ENTRY_MODIFY);
                            return FileVisitResult.CONTINUE;
                        }
                    });

                    // start the file watcher thread below
                    ScipioWatchQueueReader fileListener = new ScipioWatchQueueReader(folderWatcher,name,location);
                    ScheduledExecutorService executor = ExecutionPool.getScheduledExecutor(FILE_LISTENER_THREAD_GROUP, "filelistener-startup", Runtime.getRuntime().availableProcessors(), 0, true);
                    try {
                        executor.submit(fileListener, name);
                    } finally {
                        executor.shutdown();
                    }
                    Debug.logInfo("Starting FileListener thread for "+name, module);
                }
            }
        }catch(Exception e){
            Debug.logError("Could not start FileListener "+name+" for "+location+"\n"+e, module);

        }
    }


    /**
     * Runnable used to listen for file changes and interpret the result
     *
     * */
    private static class ScipioWatchQueueReader implements Runnable {

        private WatchService scipioWatcher;
        private String eventName,eventRoot;

        public ScipioWatchQueueReader(WatchService scipioWatcher,String eventName, String eventRoot) {
            this.scipioWatcher = scipioWatcher;
            this.eventName = eventName;
            this.eventRoot = eventRoot;
        }


        @Override
        public void run() {
            try {
                WatchKey key = scipioWatcher.take();
                while(key != null) {
                    for (WatchEvent<?> event : key.pollEvents()) {
                        GenericValue userLogin = EntityQuery.use(delegator).from("UserLogin").where("userLoginId", "system").queryOne();
                        final Path filename = (Path) event.context();
                        Path dir = (Path) key.watchable();
                        Path fullPath = dir.resolve(filename);
                        String fileType = Files.probeContentType(fullPath);
                        if(UtilValidate.isEmpty(fileType))fileType= FilenameUtils.getExtension(fullPath.toString());
                        dispatcher.runSync("triggerFileEvent", UtilMisc.toMap("fileEvent",""+event.kind(),"fileLocation",""+fullPath,"fileType",fileType,"eventName",eventName,"eventRoot",eventRoot,"userLogin",userLogin));
                    }
                    key.reset();
                    key = scipioWatcher.take();
                }
            } catch (Exception e) {
                Debug.logWarning("Could not fire FileListener Event"+e, module);
            }
        }
    }

    public static Thread getThreadByName(String threadName) {
        for (Thread t : Thread.getAllStackTraces().keySet()) {
            if (t.getName().equals(threadName)) return t;
        }
        return null;
    }
}
