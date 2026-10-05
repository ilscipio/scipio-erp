-- SCIPIO: 4.0.0: Pooled runtime: first state of a new store database. provisionTenant runs this once, on the copy of
-- the template (PostgreSQL). general.properties tenant.provision.initSql names this file.
-- One statement per line end with a semicolon; lines that start with -- are comments.

-- jobs queued by the template's own data load are not the store's work (Bing IndexNow per product)
DELETE FROM job_sandbox WHERE service_name = 'submitProductToBingIndex'
  AND (status_id IS NULL OR status_id IN ('SERVICE_PENDING', 'SERVICE_QUEUED'));
-- a job that a stopped JVM had claimed in the template runs again as a normal pending job
UPDATE job_sandbox SET status_id = 'SERVICE_PENDING', run_by_instance_id = NULL
 WHERE status_id IN ('SERVICE_QUEUED', 'SERVICE_RUNNING');
-- past-due maintenance jobs: spread over the next 1 to 24 hours, not all at the first activation (W0-03 job storm)
UPDATE job_sandbox SET run_time = now() + interval '1 hour' + random() * interval '23 hours'
 WHERE (status_id IS NULL OR status_id = 'SERVICE_PENDING') AND run_time < now()
   AND coalesce(event_id, '') <> 'SCH_EVENT_STARTUP';
-- the store gets its own Solr core (G8); the template's status describes the template's index
UPDATE solr_status SET data_status_id = 'SOLR_DATA_OLD' WHERE solr_id = 'SOLR-MAIN';
