# Development with Devspaces

### Devspaces 

Manage your Devspaces https://www.devspaces.io/.

Read up-to-date documentation about cli installation and operation in https://www.devspaces.io/devspaces/help.

Here follows the main commands used in Devspaces cli. 

|action   |Description                                                                                   |
|---------|----------------------------------------------------------------------------------------------|
|`devspaces --help`                    |Check the available command names.                               |
|`devspaces create [options]`          |Creates a DevSpace using your local DevSpaces configuration file |
|`devspaces start <devSpace>`          |Starts the DevSpace named \[devSpace\]                           |
|`devspaces bind <devSpace>`           |Syncs the DevSpace with the current directory                    |
|`devspaces info <devSpace> [options]` |Displays configuration info about the DevSpace.                  |

Use `devspaces --help` to know about updated commands.

#### Development flow

You should have Devspaces cli services started and logged to develop with Devspaces.
The following commands should be issued from **project directory**.

1 - Create Devspaces

```bash
$ cd devspaces/docker
$ devspaces create
$ cd ../../

```

2 - Start containers

```bash
devspaces start scipio-erp
```

3 - Start containers synchronization

```bash
devspaces bind scipio-erp
```

4 - Grab some container info

```bash
devspaces info scipio-erp
```

Retrieve published DNS and endpoints using this command

5 - Connect to development container

```bash
devspaces exec scipio-erp
```

6 - Configure Scipio Development environment

```bash
./install.sh
```


Select option `1` in install menu.

7 - Run Scipio

```bash
./start.sh
```

Access application URLs:

Using information retrieved in step 4, access the following URL's:

* Application (bound to port 80): 
    * http://scipio-erp.<devspaces-user>.devspaces.io:<published-ports>/shop
    * http://scipio-erp.<devspaces-user>.devspaces.io:<published-ports>/
    

8 - Stop Scipio

```bash 
./stop.sh
```

In a second terminal window.

9 - Clean 

```bash
./ant clean-all
```


### Docker Script Manager (CLI)

Currently, we have these command available to work using local docker compose.

```bash
devspaces/docker-cli.sh <command>
```

|action    |Description                                                               |
|----------|--------------------------------------------------------------------------|
|`build`   |Builds images                                                             |                                      
|`deploy`  |Deploy Docker compose containers                                          |
|`undeploy`|Undeploy Docker compose containers                                        |
|`start`   |Starts Docker compose containers                                          |
|`stop`    |Stops Docker compose containers                                           |
|`exec`    |Get into the container                                                    |

#### Development flow

1 - Build and Run `docker-compose` locally.

```bash
devspaces/docker-cli.sh build
devspaces/docker-cli.sh deploy
devspaces/docker-cli.sh start
```

2 - Get into container

```bash
devspaces/docker-cli.sh exec
```

4 - Configure Scipio Development environment

```bash
./install.sh
```


Select option `1` in install menu.

5 - Run Scipio

```bash
./start.sh
```

Access application URLs:

* Application: 
    * http://localhost/
    * http://localhost/shop
    * http://localhost:8080/shop
* Admin:
    * https://localhost:8443/admin

6 - Stop Scipio

```bash 
./stop.sh
```

In a second terminal window.

7 - Clean

```bash
./ant clean-all
```

**Obs.:** Clean script ran in docker compose may leave some files behind, that may result in files owned by `root`user in your host OS. 
You may complement the cleanup with the scripts below:
```bash
 # seek 
ls -dl `find . -type d` | grep root | awk '{print $(NF)}'

 # seek and destroy
 ls -dl `find . -type d` | grep root | awk '{print $(NF)}' | xargs sudo rm -rf
```
 
### Entrypoint Actions

    1 - NGINX is started up in background
