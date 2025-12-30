#!/usr/bin/env bash

ls *.rkt | entr racket "day$1.rkt"
