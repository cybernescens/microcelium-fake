#!/bin/bash
wd=`pwd`
faked=$wd/.fake

if [ ! -d $faked ] || [ -z "$(ls -A $faked)" ]
then
    dotnet tool install fake-cli --tool-path $faked --version 5.*
fi

$faked/fake run build.fsx "$@"
