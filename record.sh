#!/bin/sh -e

make record

exec ./record $@
