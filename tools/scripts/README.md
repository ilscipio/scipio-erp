# Scipio ERP init / systemd files

Linux utilizes system and service managers to automatically or manually manage the systems processes. Scipio ERP can be integrated whether the operating system uses an init or systemd compatible manager.

## Systemd service file

scipio.service

Follow the steps to properly install the service file:

1. Create the user Scipio ERP shall use and adjust the paths to your needs.
2. Copy scipio.service file:
```
sudo cp scipio.service /etc/systemd/system/
```

3. Enable the service if desired with:
```
sudo systemctl enable scipio.service
```

## UNIX System V init scripts

rc.scipio
rc.scipio.for.debian
rc.scipio.for.ubuntu
