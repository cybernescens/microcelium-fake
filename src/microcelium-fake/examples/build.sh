#!/bin/bash

wd=`pwd`
faked=$wd/.fake
microceliumd=$wd/.microcelium
coverletd=$wd/.coverlet

if [ ! -d $faked ] || [ -z "$(ls -A $faked)" ]
then
    dotnet tool install fake-cli --tool-path $faked --version 5.* --add-source https://api.nuget.org/v3/index.json
fi

if [ ! -d $microceliumd ] || [ -z "$(ls -A $microceliumd)" ]
then
    dotnet tool install microcelium-fake --tool-path $microceliumd --version 1.*
fi
if [ ! -d $coverletd ] || [ -z "$(ls -A $coverletd)" ]
then
    dotnet tool install dotnet-reportgenerator-globaltool --tool-path $coverletd --add-source https://api.nuget.org/v3/index.json
fi

$microceliumd/microcelium-fake -q
$faked/fake run build.fsx "$@"
