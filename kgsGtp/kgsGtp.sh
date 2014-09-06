#!/bin/sh -e

java -jar kgsGtp-3.5.11/kgsGtp.jar kgsGtp.conf name=$NAME password=$PASSWORD "$@"
