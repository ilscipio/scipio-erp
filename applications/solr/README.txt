=====================================================
Solr Component for SCIPIO ERP
=====================================================

This document describes the Scipio Solr component, which wraps and provides access to a powerful 
Apache Solr indexing and search software. It is based on the Solr component developed by Ilscipio 
that was originally submitted to the Ofbiz project many years ago (around Solr 4).
Please note that the Scipio Solr component may differ substantially with large improvements 
from the one now provided in the Ofbiz plugins project.

Currently, the component implements Solr 6. Its native documentation can be found at:

  https://lucene.apache.org/solr/guide/6_6/

This stock component focuses on indexing Product data.


Contents:

1. Configuration
2. Authentication
3. Interfaces
4. Data Indexing
5. Data Querying
6. Schema Modification
7. Troubleshooting
8. Known Bugs, Limitations and Issues


-----------------------------------------------------
1. Configuration
-----------------------------------------------------

In Scipio, Solr is already installed and enabled for use out-of-the-box.

Solr is pre-configured to index all Product data (using the rebuildSolrIndexAuto service)
on first server startup (after the initial database seeding), once the webapps are available
and loaded. Afterward, the indexing status is checked at every server startup and if the indexed
data is marked dirty, all products are reindexed. This is recorded using the special SolrStatus entity.

In addition, Solr ECAs are enabled by default, and will attempt to index Product data upon
product entity changes. However, note that this is a best-effort coverage of entity modifications, 
and client projects may need to add/modify Solr ECAs to meet their needs.
NOTE: 2017-12: The Solr ECA system was recently significantly overhauled, and now performs much
more reliably than the old component.

* Scipio configurations:
 * System properties:
  * ofbiz.solr.eca.enabled - Global solr ECA toggling boolean (true/false, see Data Indexing)
 * Config files:
  * config/solrconfig.properties - Scipio solr service behavior control
  * ofbiz-component.xml - Standard Scipio component config

* Apache Solr configurations:
 * Config files:
  * solr.xml - Base solr config
  * configsets/*/conf/managed-schema - Solr index schema
  * webapp/WEB-INF/web.xml - Dual Scipio/Solr webapp config


-----------------------------------------------------
2. Authentication
-----------------------------------------------------

Currently (2018-05-08), the /solr webapp is openly accessible in stock Scipio installations.
On production systems, it is extremely important not to expose this webapp to the
public-facing network.

On production environments the /solr webapp should be denied from public access.
In addition, it is possible to configure basic HTTP authentication so safe
access from within intranet, using one of the methods below.


2.1 Solr Basic HTTP authentication

Basic Solr authentication - independent from the Scipio login system and users - can be configured
following:

  https://lucene.apache.org/solr/guide/6_6/basic-authentication-plugin.html

It requires specifying a pair of logins in solrconfig.properties and in the file (you may need to create it):

  applications/solr/security.json
  
Example:

The following defines a few users (two admins, one query user, one update user), all having 
the same password, 'SolrRocks':

security.json (from security_solrbasicauth_disabled.json):

{
"authentication":{
   "blockUnknown": true,
   "class":"solr.BasicAuthPlugin",
   "credentials":{"admin":"IV0EHq1OnNrj6gvRCwvFwTrZ1+z1oBbnQdiVC3otuq0= Ndd7LKvVBAaZIF0QAVi1ekCfAJXr1GGfLtRUXhgrF8c=",
      "solradmin":"IV0EHq1OnNrj6gvRCwvFwTrZ1+z1oBbnQdiVC3otuq0= Ndd7LKvVBAaZIF0QAVi1ekCfAJXr1GGfLtRUXhgrF8c=",
      "solrq":"IV0EHq1OnNrj6gvRCwvFwTrZ1+z1oBbnQdiVC3otuq0= Ndd7LKvVBAaZIF0QAVi1ekCfAJXr1GGfLtRUXhgrF8c=",
      "solru":"IV0EHq1OnNrj6gvRCwvFwTrZ1+z1oBbnQdiVC3otuq0= Ndd7LKvVBAaZIF0QAVi1ekCfAJXr1GGfLtRUXhgrF8c="}
},
"authorization":{
   "class":"solr.RuleBasedAuthorizationPlugin",
   "permissions":[{"name":"read","role":"*"},
      {"name":"schema-read","role":"*"},
      {"name":"config-read","role":"*"},
      {"name":"collection-admin-read","role":"*"},
      {"name":"metrics-read","role":"*"},
      {"name":"core-admin-read","role":"*"},
      {"path":"/admin/ping","collection":null,"role":"*"},
      {"name":"update","role":["update","admin"]},
      {"name":"security-edit","role":"admin"},
      {"name":"all","role":"admin"}],
   "user-role":{"solradmin":"admin","admin":"admin","solru":"update","solrq":"query"}
}}

solrconfig.properties:

solr.query.connect.login.username=solrq
solr.query.connect.login.password=SolrRocks
solr.update.connect.login.username=solru
solr.update.connect.login.password=SolrRocks

You may, however, want to define separate users for internal query/update connections 
versus /solr webapp UI user access.


2.2 Scipio UserLogin Basic HTTP authentication

Instead of Solr-only credentials, it is now possible to use Scipio entity UserLogin
authentication using the ScipioUserLoginAuthPlugin plugin. This gives the same
prompt as the Solr basic HTTP authentication but using the UserLogin entity
and providing support for external login keys (auto login from /admin).

It allows only login to users having OFBTOOLS_* and SOLRADM_* permissions 
(as defined in ofbiz-component.xml).

In security.json, the "cacheLogins" should be specified and is meant mainly for 
the one or two query/update accounts specified in solrconfig.properties below.
It provides an additional cache to session management, and it becomes needed if the 
Solr HTTP client (Apache HttpClient) has cookies/sessions disabled or is configured 
with reuseClient=false.

security.json (from security_scipiouserlogin_disabled.json):

{
"authentication":{
   "blockUnknown": true,
   "class":"com.ilscipio.scipio.solr.plugin.security.ScipioUserLoginAuthPlugin",
   "cacheLogins":{"admin":true},
   "cachedLoginExpiry":3600000
},
"authorization":{
   "class":"solr.RuleBasedAuthorizationPlugin",
   "permissions":[{"name":"security-edit",
      "role":"admin"}],
   "user-role":{"solr":"admin"}
}}


solrconfig.properties:

solr.query.connect.login.username=admin
solr.query.connect.login.password=scipio
solr.update.connect.login.username=admin
solr.update.connect.login.password=scipio

WARNING: The example above is for example purposes only; using the admin account
as the solr query/update connection account in the properties above is NOT recommended.


-----------------------------------------------------
3. Interfaces
-----------------------------------------------------


* Scipio Solr status and control screens:

  https://localhost:8443/admin/control/SolrStatus
  (substitute 8443 with your server's http port)
  
Allows to view Scipio-side status information about the Solr webapp (to see if accessible/working)
and trigger indexing and other services.


* Native Solr interface:

  http://localhost:8080/solr
  (substitute 8080 with your server's http port)

This accesses the native Solr webapp itself.

Please refer to the Apache Solr documentation for usage of this interface
and other native Solr configuration details. Note that not all native Solr functions
are available due to running within Scipio (such as logging control).


-----------------------------------------------------
4. Data Indexing
-----------------------------------------------------

The solr component indexes data such as Products into the Apache Solr database
using services defined in the file:

  servicesdef/solrservices.xml

There are two methods for indexing data:


4.1 Index rebuilding service (rebuildSolrIndex/rebuildSolrIndexAuto)

rebuildSolrIndex is the most important data import service. It reindexes all Scipio Products existing
in the system into the solr index. rebuildSolrIndexAuto is a wrapper that invokes rebuildSolrIndex if
the data is marked dirty, the schema appears to have changed or other conditions.

In Scipio, a rebuildSolrIndexAuto job runs once automatically after initial data load 
and again every server startup if the data appears to have been marked dirty (through SolrStatus entity)
or when the "solr.config.version"/"solr.config.version.custom" property from solrconfig.properties is
detected to have changed (from the last full indexing, through through SolrStatus entity).

These services can be triggered manually or by scheduling a job through the backend:

  https://localhost:8443/admin/control/SolrServices

It can also be requested on the command line (this forces the startup-time reindex, even if not marked dirty):

  ./ant start-reindex-solr
  
The Solr data is marked "dirty" by the Scipio Solr component in several situations, notably on errors
or whenever product data is changed but the invoked Solr ECAs/SECAs cannot be executed in context 
(assuming you haven't commented out those ECAs/SECAs - see below).


4.2 ECAs/SECAs (registerUpdateToSolr service)

Although the rebuildSolrIndex is always necessary for the initial data import, one may also
use ECAs and SECAs to import subsequent data changes automatically at every individual data (e.g. Product)
update instead of running rebuildSolrIndex periodically.

This is done by defining ECAs or SECAs that trigger the registerUpdateToSolr service, which reindexes
a single Product and its associated data.

In Scipio, Solr ECAs are enabled by default. Some projects may want to disable these, which can be done
by setting "solr.eca.enabled=false" in solrconfig.properties, or by passing the system property
"ofbiz.solr.eca.enabled=false" on the command line (./ant start -Dofbiz.solr.eca.enabled=false).

Some stock/default/example ECAs/SECAs are provided and cover a number of product changes, in a best-effort
fashion. Client projects may need to modify or augment these for their own needs. They can be found in:

  entitydef/eecas.xml
  servicedef/secas.xml

NOTE: In the current implementation (2017-12), the Scipio will log info-level Solr ECA service calls 
even when Solr ECAs are disabled with "solr.eca.enabled=false"; this has little effect on functionality.
It is simply that the service engine is a bit verbose. You may, alternatively, simply comment out
all the ECAs/SECAs or their file includes in ofbiz-component.xml; however this may negatively affect
dirty-data detection and you then have to be doubly certain about your plan to schedule 
rebuildSolrIndex/rebuildSolrIndexAuto invocations. 

***

To reset and clear the solr index completely, simply run the following Ant task in the main project directory:

  ./ant clean-search-indexes
  
In addition, they are now (2018-04-26) cleared by the "clean-all" task.


-----------------------------------------------------
5. Data Querying
-----------------------------------------------------

Solr queries can be done using severals methods:


* Solr Scipio services:

Simply invoke (manually or in code) the query services found in the file:

  servicesdef/solrservices.xml
  
These include solrProductsSearch, solrKeywordSearch and others. Note that in general,
solr services can only successfully run in contexts where the solr webapp is loaded and accessible.

For developers, the Scipio Shop contains a developed keyword search implementations in the file:

  /applications/shop/webapp/shop/WEB-INF/actions/shop/KeywordSearch.groovy


* Solr native admin webapp interface:

  http://localhost:8080/solr
  (substitute 8080 with your server's http port)
  
One can also perform native Solr queries and diagnostics using the standard admin interface, 
accessible as described under Configuration. Please refer to the Apache Solr documentation for usage 
of this interface.


-----------------------------------------------------
6. Schema Modification
-----------------------------------------------------

For Solr 6, the schemas are now defined for each configset in:

  configsets/*/conf/managed-schema
  
  (this is an XML file; it has no extension)
  
This file may be edited by hand WHILE THE SERVER IS STOPPED (./ant stop).
  
In Scipio, for most projects, despite ominous warnings from parts of the Solr documentation,
it is NOT recommended to use the UI interface to modify the Solr schema, because the file
is then restructured and all comments and formatting are lost. In fact, it is perfectly safe
to edit the file by hand AS LONG AS the server is stopped (the following is a quote):

  https://lucene.apache.org/solr/guide/6_6/schema-api.html#schema-api
  
    "Why is hand editing of the managed schema discouraged?

    The file named "managed-schema" in the example configurations may include a note that recommends 
    never hand-editing the file. Before the Schema API existed, such edits were the only way to make 
    changes to the schema, and users may have a strong desire to continue making changes this way.

    The reason that this is discouraged is because hand-edits of the schema may be lost if the 
    Schema API described here is later used to make a change, unless the core or collection is reloaded 
    or Solr is restarted before using the Schema API. If care is taken to always reload or restart after 
    a manual edit, then there is no problem at all with doing those edits."

Whenever the schema is changed, you must run the rebuildSolrIndex service on next server startup
or as soon as possible. You may do this manually or, if you work with other developers, you can 
choose to manage the "solr.config.version.custom" property in solrconfig.properties; whenever 
this is increased, the stock Scipio Solr setup will trigger a rebuildSolrIndex call on server 
startup, so that you don't need to tell other users of your (git) project when to rebuild the index.


-----------------------------------------------------
7. Troubleshooting
-----------------------------------------------------

7.1 Timeouts and internal connection failures to Solr:

Check the solr.query.connect.* and solr.update.connect.* settings in solrconfig.properties;
under extremely heavy loads, pooling settings (reuseClient + maxConnections*) may need to be tweaked.
Debug logging for Apache HttpClient (used for Solr connections) can be enabled by
uncommenting the appropriate line in:

  framework/base/config/log4j2.xml


-----------------------------------------------------
8. Known Bugs, Limitations and Issues
-----------------------------------------------------

* In general, solr services can only successfully run in contexts where the solr webapp
  is loaded and accessible. This means it is impossible to index data during load-demo/load-* targets.
* On fast systems, the rebuildSolrIndexAuto service may fail to connect to Solr on server startup 
  when Solr is slower to load than Scipio, which will cause errors in the log; however this is already
  accounted for and the service should reschedule and attempt another check/reindex every few moments
  thereafter.

***

Please report any other issues encountered, or ask any further questions, on the SCIPIO ERP forum:
  
  https://forum.scipioerp.com/
  
Thank you.



