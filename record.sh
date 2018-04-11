#!/bin/sh -e

make

exec ./finances record $@
