#!/usr/bin/env bash

tar -xzf cpdt-2019-08-17.tgz 
mv cpdt cpdt-book
cd cpdt-book
make -j
