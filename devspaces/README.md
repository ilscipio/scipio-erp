# Development with Devspaces

## Minimum Requirements

* Python3 (>= 3.5)
* pip3 (>=9.0.1)
* Docker
* Docker-compose
* Virtualenv

## Devspaces Configuration

Verify that you have access to https://devspaces.ey.devfactory.com (it requires "jira-users" access into AD).

Follow the steps in Devspaces Installation Guide for detailed instructions: http://devspaces-docs.ey.devfactory.com/installation/index.html. Windows 10 users may use the quick install guide below.

### Quick Install Guide For Windows 10 Users

1. Please begin with [Ubuntu WSL install on Windows](https://www.howtogeek.com/249966/how-to-install-and-use-the-linux-bash-shell-on-windows-10/). (*choose Ubuntu 18.04+*)
1.1. Have a little guidance on [Ubuntu WSL File Access](https://www.howtogeek.com/261383/how-to-access-your-ubuntu-bash-files-in-windows-and-your-windows-system-drive-in-bash/)
2. Inside Ubuntu WSL Install `python`, `pip3` and `virtualenv`. [(Detailed Python installation - if needed)](https://linoxide.com/linux-how-to/setup-python-virtual-environment-ubuntu/):
    1. `sudo apt update`
    2. `sudo apt install python3`
    3. `sudo apt install python3-pip`
    4. `sudo apt install python3-venv`
3. Install `docker` inside Ubuntu WSL using [latest docker intallation reference](https://docs.docker.com/install/linux/docker-ce/ubuntu/#install-docker-ce)
4. Install `docker-compose` inside Ubuntu WSL using [latest docker-compose installation reference](https://docs.docker.com/compose/install/)

If you want to setup your custom Devspaces:

* http://devspaces-docs.ey.devfactory.com/quickstart.html

## Devspaces Operation

### Devspaces Script Manager (CLI)

Currently, we have these command available to work with Devspaces

```bash
devspaces/devspaces <command>
```

|action   |Description                                                               |
|---------|--------------------------------------------------------------------------|
|`help`   |Check the available command names.                                        |
|`start`  |Setup Devspaces in case it's not installed and start the complete infrastructure required to work with Devspaces |
|`exec`   |Get into the container                                                    |
|`reset`  |Reset the data.                                                           |
|`info`   |Retrieve the URLs to access each of the deployed products.                |
|`stop`   |Shutdown the Devspaces infrastructure and unbind all the bound folders.   |

#### Development flow

1 - Start containers

```bash
devspaces/devspaces start
```

2 - Grab some container info

```bash
devspaces/devspaces info
```

3 - Connect to development container

```bash
devspaces/devspaces exec
```

4 - Install theme in running background SICM instance (From `/data` dir in container)

```bash
gradle themeDev
```

**Obs.:** It may some  minutes until SICM instance finishes all pre-installed themes compilation when run by first time. Before theme installation, you may test the availability of SICM port by running this command inside container: `curl 127.0.0.1:9000`.

5 - Insert this entry in your local workstation `/etc/hosts` file replacing the `SCIM_IP` value with IP given by `devspaces/devspaces info` and the `THEME_NAME` value by theme installed in SICM:

```bash
<SCIM_IP>  <THEME_NAME>.127-0-0-1.org.uk
```

**Obs.:** Besides this hosts configuration you should use the PORT published by Devspaces. Use `devspaces/devspaces info` to get the published port.

**Obs. 2:** **Windows** users should update this file: `c:\Windows\System32\Drivers\etc\hosts` (requires admin rights). Alternatively, Google **Chrome** users can use a `DNS Overrider` plugin (Use **Google Web Store** search tool to install it).

6 - Restart SICM process if needed

```bash
supervisorctl stop play
supervisorctl start play
```

### Docker Script Manager (CLI)

Currently, we have these command available to work using local docker compose.

```bash
devspaces/docker-cli.sh <command>
```

|action    |Description                                                               |
|----------|--------------------------------------------------------------------------|
|`build`   |Builds all images                                                         |
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

4 - Install theme in running background SICM instance (From `/data` dir in container)

```bash
gradle themeDev
```

**Obs.:** it's not required to change `/etc/hosts`

**Obs. 2:** It may some  minutes until SICM instance finishes all pre-installed themes compilation when run by first time. Before theme installation, you may test the availability of SICM port by running this command inside container: `curl 127.0.0.1:9000`.

### Entrypoint Actions

When a new Placeable Pages (SICM) container is launched whether by CN Devspaces collections or by provided Docker-compose the following actions are performed once:

    1 - SICM PLAY application is started up in background

### Docker Image

 `registry2.swarm.devfactory.com/ignite/placeable-pages-sicm-themes-buildenv:0.1`

